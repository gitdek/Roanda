/*
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
#include <zmq.h>
#include <czmq.h>
#include <cuda.h>
#include <cudaProfiler.h>
#include "util.h"
#include "sha1.h"
#include "rsa.h"
#include "x509.h"
#include "dkey.h"
#include "dkim.h"

/* Structure to be passed between the CPU and GPU, conveying the results of the signing process */
typedef struct metastore
{
  uint32_t used;
  uint32_t length[TOTALMESSAGES];
  uint32_t offset[TOTALMESSAGES];

  char taskid[TOTALMESSAGES][SMALLSTRLEN];
  uint8_t signatures[TOTALMESSAGES];
  uint8_t sigtype[TOTALMESSAGES][2];
  char signature[TOTALMESSAGES][2][SMALLSTRLEN];

} metastore;

/* Structure to keep track of a CUDA stream and its associated message blocks */
typedef struct streamstore
{
  uint32_t device;
  uint32_t streamid;
  char *host_message_mem;
  metastore *host_meta_mem;
  char *card_message_mem;
  metastore *card_meta_mem;
  cudaStream_t stream;
  sem_t writesem;
} streamstore;

streamstore * streams;            /* Pointer to all streams */
uint32_t totalstreams = 0;        /* Number of streams based on number of devices */
uint32_t curstream = 0;            /* Index of stream being written */
pthread_mutex_t transferlock;    /* Only allow one thread to change the current stream */

int workertype = 0;                /* CPU=0 or GPU=1 */
char nodeid[SMALLSTRLEN];            /* Worker identity passed on command line */
char controller[SMALLSTRLEN];        /* ZeroMQ path for communicating with controller node */

FILE * logfileh = stdout;        /* Handle for writing log messages */
int threadcount = 0;            /* Number of signing threads to run (CPU nodes only) */
int devicecount = 0;            /* Numner of CUDA devices available */
queue ** waitingqueues = NULL;    /* Queues to hold messages waiting to be signed */
queue * controllerqueue = NULL;    /* Queue to hold messages for sending to the controller */
uint32_t apprunning = 1;        /* Global status to track whether application should still be running */
time_t lastkeepalive = 0;        /* Time that keepalive message was last sent to controller */

/* Function to message to the controller node to signify that the worker is still running */
/* Messages are sent at a maximum frequency of once a second */
int send_keepalive(char * nodeid, int workertype, void * socket)
{
  if((time(NULL) - lastkeepalive) > 0)
  {
    lastkeepalive = time(NULL);

    /* Build task from arguments */
    uint32_t total_size = 0, offset = 0;
    uint8_t message_type = TYPE_KEEPALIVE_REQUEST;
    uint8_t nodeid_len = strlen(nodeid) + 1;

    total_size = total_size + sizeof(uint8_t);                    /* Account for message type value */
    total_size = total_size + sizeof(uint8_t) + nodeid_len;     /* Account for node id length and value */
    total_size = total_size + sizeof(uint8_t);                    /* Account for node type value */
    total_size = total_size + sizeof(uint8_t);                    /* Account for thread or device count */

    zmq_msg_t message;
    zmq_msg_init_size (&message, total_size);
    char * msg_data = (char *)zmq_msg_data(&message);

    /* Field length = 4, Field value = 0 (Register) */
    write_uint8_value(msg_data, &offset, total_size, message_type);

    /* Node ID */
    write_uint8_length_value(msg_data, &offset, nodeid_len, total_size, nodeid);

    /* Worker type */
    write_uint8_value(msg_data, &offset, total_size, workertype);

    /* Thread or device */
    write_uint8_value(msg_data, &offset, total_size, workertype ? devicecount: threadcount);

    /* Send message */
    int size_sent = zmq_msg_send (&message, socket, 0);
    if(size_sent == -1)
    {
      host_log_printf(logfileh, "Error: Failed to send keepalive message to controller\n");
      return 1;
    }
    zmq_msg_close (&message);

    if(size_sent != total_size)
    {
      host_log_printf(logfileh, "Error: Failed to send message to controller\n");
      return 1;
    }
  }

  return 0;
}

/* Callback function that is run after a stream of signing requests is complete */
void stream_callback(cudaStream_t stream, cudaError_t status, void *data)
{
  if(apprunning == 0) return;

  streamstore * ss = (streamstore *)data;
  metastore * hms = ss->host_meta_mem;

  host_log_printf(logfileh, "Info: Sending responses from stream %i...\n", ss->streamid);

  /* Build a signing response message for each task and put onto the controller queue */
  int resi;
  for(resi = 0; resi < hms->used; resi++)
  {
    task * taskresponse = (task *)calloc(1, sizeof(struct task));
    if(taskresponse == NULL)
    {
      host_log_printf(logfileh, "Error: Unable to allocate memory for task\n");
      return;
    }

    taskresponse->type = TYPE_SIGN_RESPONSE;
    strcpy(taskresponse->id, hms->taskid[resi]);
    taskresponse->id_len = strlen(taskresponse->id) + 1;

    /* Populate signatures from returned results */
    int sigi;
    for(sigi = 0; sigi < hms->signatures[resi]; sigi++)
    {
      if((hms->sigtype[resi][sigi] & DKIM_MODE) != 0)
      {
        strcpy(taskresponse->dkim, hms->signature[resi][sigi]);
        taskresponse->dkim_len = strlen(taskresponse->dkim) + 1;
      }
      if((hms->sigtype[resi][sigi] & DKEY_MODE) != 0)
      {
        strcpy(taskresponse->dkey, hms->signature[resi][sigi]);
        taskresponse->dkey_len = strlen(taskresponse->dkey) + 1;
      }
    }

    add_to_queue(controllerqueue, taskresponse);
  }

  /* Clear host memory for next use */
  memset(ss->host_meta_mem, 0, sizeof(struct metastore));
}

/* Kernel function to be run on CUDA capable card */
/* Function processes one message, producing DKEY and/or DKIM signatures depending on request */
__global__ void signmessagekernel(void * messagedatain, void * metadatain, time_t signtime)
{
  /* Find message to be signed based on block and thread IDs */
  metastore * metadata = (metastore *)metadatain;
  uint32_t index = (blockIdx.x * MESSAGESPERBLOCK) + threadIdx.x;

  /* Check there is a message waiting in the area allocated to this thread */
  if(index < metadata->used)
  {
    char * messagedata = (char *)messagedatain + metadata->offset[index];

    task newtaskp;
    memset(&newtaskp, 0, sizeof(struct task));

    uint32_t offset = 0;

    /* Populate task structure from raw message block */
    if(read_sign_request(messagedata, &offset, metadata->length[index], &newtaskp) != 0)
    {
      device_log_printf("Error: Failed to read signing request\n");
      return;
    }

    /* Set attributes for response */
    if(cuda_strncpy(metadata->taskid[index], 256, newtaskp.id) != 0) return;

    /* Signature timestamp */
    newtaskp.ts = signtime;

    if((newtaskp.mode & DKIM_MODE) != 0)
    {
      /* Create DKIM signature */
      if(dkim_create(&newtaskp) != 0)
      {
        device_log_printf("Error: Failed to create DKIM signature\n");
        return;
      }

      /* Populate returning metadata block with calculated signature */
      metadata->sigtype[index][metadata->signatures[threadIdx.x]] += DKIM_MODE;
      cuda_strcpy((char *)(metadata->signature[index][metadata->signatures[index]]), newtaskp.dkim);
      metadata->signatures[index]++;
    }

    if((newtaskp.mode & DKEY_MODE) != 0)
    {
      /* Create DKEY signature */
      if(dkey_create(&newtaskp) != 0)
      {
        device_log_printf("Error: Failed to create DKEY signature\n");
        return;
      }

      /* Populate returning metadata block with calculated signature */
      metadata->sigtype[index][metadata->signatures[index]] += DKEY_MODE;
      cuda_strcpy((char *)(metadata->signature[index][metadata->signatures[index]]), newtaskp.dkey);
      metadata->signatures[index]++;
    }
  }
}

/**
* This macro checks return value of the CUDA runtime call and exits
* the application if the call failed.
*/
#define CUDA_CHECK_RETURN(value) {                                              \
cudaError_t _m_cudaStat = value;                                                \
if (_m_cudaStat != cudaSuccess) {                                               \
  host_log_printf(logfileh, "Error: %s at line %d in file %s\n",                \
    cudaGetErrorString(_m_cudaStat), __LINE__, __FILE__);                       \
  return 1;                                                                     \
} }

/* Function called whenever there is a full block of messages ready to be deployed to the CUDA card */
/* A block is full when either the maxmimum number of messages is populated, or the culmulative size of the messages reaches the maximum block size */
int send_block(streamstore * ss, metastore * hms)
{
  host_log_printf(logfileh, "Info: Signing stream %i on device %i with %i messages...\n", curstream, ss->device, hms->used);

  cudaSetDevice(ss->device);

  /* Allocate space for tasks on card and copy over */
  CUDA_CHECK_RETURN(cudaMalloc((void**) &(ss->card_message_mem), MESSAGESIZE * TOTALMESSAGES));
  CUDA_CHECK_RETURN(cudaMemcpyAsync(ss->card_message_mem, ss->host_message_mem, MESSAGESIZE * TOTALMESSAGES, cudaMemcpyHostToDevice, ss->stream));

  /* Allocate space for metadata on card and copy over */
  CUDA_CHECK_RETURN(cudaMalloc((void**) &(ss->card_meta_mem), sizeof(struct metastore)));
  CUDA_CHECK_RETURN(cudaMemcpyAsync(ss->card_meta_mem, ss->host_meta_mem, sizeof(struct metastore), cudaMemcpyHostToDevice, ss->stream));

  /* Run the kernel */
  signmessagekernel<<<NUMBEROFBLOCKS, MESSAGESPERBLOCK, 0, ss->stream>>>(ss->card_message_mem, ss->card_meta_mem, time(NULL));

  /* Copy back metadata */
  CUDA_CHECK_RETURN(cudaMemcpyAsync(ss->host_meta_mem, ss->card_meta_mem, sizeof(struct metastore), cudaMemcpyDeviceToHost, ss->stream));

  /* Free space used on card */
  CUDA_CHECK_RETURN(cudaFree((void*) ss->card_message_mem));
  CUDA_CHECK_RETURN(cudaFree((void*) ss->card_meta_mem));

  /* Add callback for after all these asynchronous actions have been completed */
  cudaStreamAddCallback(ss->stream, stream_callback, (void *)ss, 0);

  /* Move to next stream */
  curstream = (curstream + 1)%totalstreams;
  streams[curstream].host_meta_mem->used = 0;

  return 0;
}

/* Function to add the raw content of a request into the memory block to be transferred to the CUDA card */
/* Checks for space, transmitting current block and moving to the next block if necessary */
int add_to_transfer(zframe_t * content, uint32_t offset)
{
  pthread_mutex_lock(&transferlock);

  streamstore * ss = &(streams[curstream]);
  metastore * hms = ss->host_meta_mem;

  /* add_to_transfer is called without content in order to force sending of the block (timeout) */
  if(content == NULL)
  {
    /* Check there is actually something to send */
    if(hms->used != 0)
    {
      /* Force send */
      send_block(ss, hms);
    }
  } else {

    /* Calculate position for next message */
    if(hms->used != 0)
    {
      hms->offset[hms->used] = hms->offset[hms->used - 1] + hms->length[hms->used - 1];

      /* Check for space in current stream block */
      if(((hms->offset[hms->used] + zframe_size(content)) >= (MESSAGESPERBLOCK * MESSAGESIZE)) || (hms->used == MESSAGESPERBLOCK))
      {
        send_block(ss, hms);
      }
    }
  }

  /* Add incoming message to transfer block */
  if(content != NULL)
  {
    /* Set stream in the block */
    streamstore * ss = &streams[curstream];
    metastore * hms = ss->host_meta_mem;
    ss->streamid = curstream;

    /* Copy data */
    char * framedata = (char *)zframe_data(content);
    hms->length[hms->used] = zframe_size(content) - offset;
    memcpy(ss->host_message_mem + hms->offset[hms->used], framedata + offset, hms->length[hms->used]);

    hms->used++;

    /* Check if the block is full */
    if(hms->used == MESSAGESPERBLOCK)
    {
      send_block(ss, hms);
    }

    /* Indicate progress to prevent timeout */
    sem_post(&(streams[curstream].writesem));
  }

  pthread_mutex_unlock(&transferlock);

  return 0;
}

/* Extract details of signing request from the controller */
int process_controller_request(zframe_t * content, int threadnum)
{
  /* Create a task structure to carry the details of this request */
  task * newtask = (task *)calloc(1, sizeof(task));
  if(newtask == NULL)
  {
    host_log_printf(logfileh, "Error: Unable to allocate memory for task\n");
    return 1;
  }
  memset(newtask, 0, sizeof(struct task));

  /* Parse request from client */
  uint32_t offset = 0;
  char * framedata = (char *)zframe_data(content);

  /* Check message size is within allowable limit */
  if(zframe_size(content) > ((TOTALMESSAGES-1) * MESSAGESIZE))
  {
    host_log_printf(logfileh, "Error: Message is larger then maximum permitted message size\n");
    zframe_destroy(&content);
    free(newtask);
    return 1;
  }

  /* Read message type */
  if(read_uint8_value(framedata, &offset, zframe_size(content), &(newtask->type)) != 0)
  {
    host_log_printf(logfileh, "Error: Failed to read type of request received from controller\n");
    zframe_destroy(&content);
    free(newtask);
    return 1;
  }

  switch(newtask->type)
  {
  case TYPE_SIGN_REQUEST:
  {
    if(workertype == 0)
  {
    /* CPU based node - Extract task details and add to queue */
    newtask->content = content;
    if(read_sign_request((char *)zframe_data(newtask->content), &offset, zframe_size(content), newtask) != 0)
    {
      host_log_printf(logfileh, "Error: Failed to read task details\n");
      zframe_destroy(&content);
      free(newtask);
      break;
    }

    /* Add task to queue of next available thread */
    add_to_queue(waitingqueues[threadnum], newtask);

  } else {
    /* GPU based - write into memory block for transfer */
    add_to_transfer(content, offset);
    zframe_destroy(&content);
    free(newtask);
  }

  break;
  }
  default:
  {
    host_log_printf(logfileh, "Error: Unknown message type\n");
    zframe_destroy(&content);
    free(newtask);
    break;
  }
  }

  return 0;
}

/* CPU based thread for signing messages */
/* Reads requests from incoming queue, processes, and pass back to controller */
void * signmessagethread(void * param)
{
  queue * waitingqueue = (queue *)param;

  while(apprunning)
  {
    /* Wait for something to send */
    if(sem_trywait(&(waitingqueue->sendsem)) == 0)
    {
      while(waitingqueue->head != NULL)
      {
        /* Read task from front of queue */
        task * taskp = waitingqueue->head;

        /* Set timestamp for message */
        taskp->ts = time(NULL);

        if((taskp->mode & DKEY_MODE) != 0)
        {
          /* Generate DKEY signature */
          if(dkey_create(taskp) != 0)
          {
            taskp->dkey[0] = '\0';
            taskp->dkey_len = 0;
            host_log_printf(logfileh, "Error: Unable to create DKEY signature\n");
          }
        }

        if((taskp->mode & DKIM_MODE) != 0)
        {
          /* Generate DKIM signature */
          if(dkim_create(taskp) != 0)
          {
            taskp->dkim[0] = '\0';
            taskp->dkim_len = 0;
            host_log_printf(logfileh, "Error: Unable to create DKIM signature\n");
          }
        }

        /* Update message type so that it will get sent back */
        taskp->type = TYPE_SIGN_RESPONSE;

        /* Return to controller */
        move_task(taskp, waitingqueue, controllerqueue);
      }
    } else {
      usleep(10000);
    }
  }

  return NULL;
}

/* GPU based thread for signing messages */
/* Monitors activity and forces send of outstanding messages to card after inactivity */
void * gpusignmessagethread(void * param)
{
  struct timespec delay;
  delay.tv_nsec = 0;
  int writestream = 0;

  while(apprunning)
  {
    /* Wait for something to send */
    delay.tv_sec = time(NULL) + 30;
    if(sem_timedwait(&(streams[writestream].writesem), &delay) == 0)
    {
      /* Message has been written during wait - Don't need to do anything */
    } else {
      /* Timeout has occurred - force send */
      add_to_transfer(NULL, 0);
    }
  }

  return NULL;
}

/* Main thread for communicating with controller */
void * controllerthread(void * param)
{
  void * context;
  void * cc_socket;

  /* Create socket for sending messages */
  context = zmq_ctx_new ();
  if(context == NULL)
  {
    host_log_printf(logfileh, "Error: Unable to create zmq context\n");
    return NULL;
  }

  cc_socket = zmq_socket (context, ZMQ_DEALER);

  if(cc_socket == NULL)
  {
    host_log_printf(logfileh, "Error: Unable to open zmq socket\n");
    return NULL;
  }

  /* Set socket identity based on node id passed on command line */
  char * idstring = (char *)param;
  //zsockopt_set_identity (cc_socket, idstring);

  /* Don't hold on to messages if the other end drops */
  int timeout = 0;
  if(zmq_setsockopt(cc_socket, ZMQ_LINGER, &timeout, sizeof(timeout)) != 0)
  {
    host_log_printf(logfileh, "Error: Failed to set linger period\n");
    return NULL;
  }

  /* Set large queue sizes to buffer messages - argument is number of messages */
  int qsize = 100000;
  if(zmq_setsockopt(cc_socket, ZMQ_SNDHWM, &qsize, sizeof(qsize)) != 0)
  {
    host_log_printf(logfileh, "Error: Failed to set queue size\n");
    return NULL;
  }

  if(zmq_setsockopt(cc_socket, ZMQ_RCVHWM, &qsize, sizeof(qsize)) != 0)
  {
    host_log_printf(logfileh, "Error: Failed to set queue size\n");
    return NULL;
  }

  /* Establish connection to controller */
  if(zmq_connect (cc_socket, controller) != 0)
  {
    host_log_printf(logfileh, "Error: Failed to connect to listening socket\n");
    return NULL;
  }

  int thread_to_use = 0;
  zmq_pollitem_t items [] = { { cc_socket, 0, ZMQ_POLLIN, 0 } };
  while (apprunning)
  {
    /* Send update to controller to show availability */
    send_keepalive(nodeid, workertype, cc_socket);

    /* Check of any incoming messages */
    int numitems = zmq_poll (items, 1, 0);
    switch(numitems)
    {
    case -1:
    {
      host_log_printf(logfileh, "Error: Failure to poll\n");
      break;
    }
    case 0:
    {
      usleep(10);
      break;
    }
    default:
    {
      if (items[0].revents & ZMQ_POLLIN)
    {
      /* Attempt to read message */
      zmsg_t *msg = zmsg_recv (cc_socket);
      zframe_t *content = zmsg_pop (msg);
      if(content == NULL)
      {
        host_log_printf(logfileh, "Error: Failed to read content of message\n");
        zmsg_destroy (&msg);
        continue;
      }

      /* Select thread to use - round robin */
      if(threadcount != 0)
      {
        thread_to_use = (thread_to_use + 1)%threadcount;
      }

      /* Actual message processing */
      if(process_controller_request(content, thread_to_use) != 0)
      {
        host_log_printf(logfileh, "Error: Failed to process message from controller\n");
        zmsg_destroy (&msg);
        continue;
      }

      zmsg_destroy (&msg);
    }
      break;
    }
    }

    /* Check it there is something to send */
    if(sem_trywait(&(controllerqueue->sendsem)) == 0)
    {
      /* Loop through all messages waiting on the queue */
      while(controllerqueue->head != NULL)
      {
        task * taskp = controllerqueue->head;

        switch(taskp->type)
        {
        case TYPE_SIGN_RESPONSE:
        {
          /* Calculate size required for response message */
          uint32_t total_size = 0, offset = 0, signatures = 0;
          total_size = total_size + sizeof(uint8_t);                     /* Account for message type */
          total_size = total_size + sizeof(uint8_t) + taskp->id_len;     /* Account for task id */
          total_size = total_size + sizeof(uint8_t);                    /* Account for number of signatures */

          if(taskp->dkey_len > 0)
          {
            total_size = total_size + sizeof(uint8_t) + 1;                         /* Account for signature type */
          total_size = total_size + sizeof(uint16_t) + taskp->dkey_len + 1;      /* Account for signature length and value */
          signatures++;
          }

          if(taskp->dkim_len > 0)
          {
            total_size = total_size + sizeof(uint8_t) + 1;                         /* Account for signature type */
          total_size = total_size + sizeof(uint16_t) + taskp->dkim_len + 1;      /* Account for signature length and value */
          signatures++;
          }

          /* Build response message */
          zmq_msg_t message;
          memset(&message, 0, sizeof(zmq_msg_t));
          zmq_msg_init_size (&message, total_size);
          char * msg_data = (char *)zmq_msg_data(&message);
          write_uint8_value(msg_data, &offset, total_size, taskp->type);
          write_uint8_length_value(msg_data, &offset, taskp->id_len, total_size, taskp->id);
          write_uint8_value(msg_data, &offset, total_size, signatures);

          if(taskp->dkey_len > 0)
          {
            write_uint8_value(msg_data, &offset, total_size, DKEY_MODE);
            write_uint16_length_value(msg_data, &offset, taskp->dkey_len, total_size, taskp->dkey);
          }

          if(taskp->dkim_len > 0)
          {
            write_uint8_value(msg_data, &offset, total_size, DKIM_MODE);
            write_uint16_length_value(msg_data, &offset, taskp->dkim_len, total_size, taskp->dkim);
          }

          /* Send response */
          int size_sent = zmq_msg_send (&message, cc_socket, 0);
          if(size_sent == -1)
          {
            host_log_printf(logfileh, "Error: Failed to send message to controller\n");
            break;
          }
          zmq_msg_close (&message);

          if(size_sent != total_size)
          {
            host_log_printf(logfileh, "Error: Failed to send message to controller\n");
            break;
          }

          break;
        }

        default:
        {
          host_log_printf(logfileh, "Error: Request to send unrecognised message type\n");
          break;
        }
        }

        /* Remove message from queue */
        if(remove_from_queue(controllerqueue, taskp) == 0)
        {
          destroy_task(taskp);
        }
      }
    }
  }

  /* Cleanup everything if the application is closing */
  host_log_printf(logfileh, "Cleaning up sockets\n");

  zmq_close (cc_socket);
  zmq_ctx_destroy (context);

  return 0;
}

void termination_handler (int signum)
{
  apprunning = 0;
}

int main(int argc, char **argv)
{
  pthread_t srthread;
  pthread_t * signingthreads = NULL;
  int threadi;

  /* Check command line arguments */
  if((argc != 5) && (argc != 6))
  {
    logfileh = stdout;
    host_log_printf(logfileh, "Usage: worker <Type> <Worker ID> <Number of threads> <Controller location> [<Logfile>]\n");
    host_log_printf(logfileh, "               <Type> = CPU or GPU\n");
    host_log_printf(logfileh, "               <Worker ID> = Unique string to represent this worker instance\n");
    host_log_printf(logfileh, "               <Number of threads> = Concurrent signing threads to run (Relevant to CPU instances only)\n");
    host_log_printf(logfileh, "               <Controller location> = tcp://Address:Port of controlling host\n");
    host_log_printf(logfileh, "               <Log file> = Location for application logging (Console output is used if a logfile is not specified)\n");
    return 1;
  }

  /* Setup signal handling to cleanly shutdown */
  struct sigaction new_action;
  new_action.sa_handler = termination_handler;
  sigemptyset (&new_action.sa_mask);
  new_action.sa_flags = 0;
  sigaction (SIGINT, &new_action, NULL);

  pthread_mutex_init(&transferlock, NULL);

  /* Copy command line arguments that need to be visible globally */
  sprintf(nodeid, "%s-%lu", argv[2], time(NULL));
  strcpy(controller, argv[4]);

  /* Open logfile if specified */
  if(argc == 6)
  {
    logfileh = fopen(argv[5], "a");
    if(logfileh == NULL)
    {
      /* Default to stdout if logfile was not opened */
      logfileh = stdout;
      host_log_printf(logfileh, "Error: Unable to open log file '%s'. Outputting log messages to console.\n", argv[5]);
    }
  }

  /* Check what type of node we are supposed to be running */
  if(strcmp(argv[1], "GPU") == 0)
  {
    /* Check if we have any CUDA devices */
    cudaGetDeviceCount(&devicecount);
    host_log_printf(logfileh, "Info: %i CUDA devices detected\n", devicecount);

    /* Calculate memory space required for streams */
    totalstreams = STREAMSPERDEVICE * devicecount;
    streams = (streamstore *)calloc(totalstreams, sizeof(struct streamstore));
    if(streams == NULL)
    {
      host_log_printf(logfileh, "Error: Unable to allocate host memory for streams\n");
      return 1;
    }

    if(devicecount != 0)
    {
      /* Initialise a GPU based worker node */
      host_log_printf(logfileh, "Info: Initialising node as GPU worker\n");
      workertype = 1;
      threadcount = 1;

      /* Loop through all devices */
      int device;
      for (device = 0; device < devicecount; ++device)
      {
        /* Select and reset device */
        cudaSetDevice(device);
        cudaDeviceReset();

        /* Display device properties */
        cudaDeviceProp deviceProp;
        cudaGetDeviceProperties(&deviceProp, device);
        host_log_printf(logfileh, "Info: Device %d has compute capability %d.%d.\n", device, deviceProp.major, deviceProp.minor);

        /* Allocate and initialise space for raw message data and metadata */
        for (int i = 0; i < STREAMSPERDEVICE; i++)
        {
          streams[i].device = device;

          streams[i].host_message_mem = (char *)calloc(MESSAGESIZE * TOTALMESSAGES, sizeof(char));
          if(streams[i].host_message_mem == NULL)
          {
            host_log_printf(logfileh, "Error: Unable to allocate host memory for messages\n");
            return 1;
          }

          streams[i].host_meta_mem = (metastore *)calloc(1, sizeof(struct metastore));
          if(streams[i].host_meta_mem == NULL)
          {
            host_log_printf(logfileh, "Error: Unable to allocate host memory for metadata\n");
            return 1;
          }

          streams[i].host_meta_mem->used = 0;
          cudaStreamCreate(&(streams[i].stream));

          if(sem_init(&(streams[i].writesem), 0, 0) == -1)
          {
            host_log_printf(logfileh, "Error: Failed to initialise semaphore\n");
            return 1;
          }
        }
      }

      /* Start thread to monitor memory blocks */
      signingthreads = (pthread_t *)calloc(threadcount, sizeof(pthread_t));
      if(signingthreads == NULL)
      {
        host_log_printf(logfileh, "Error: Unable to allocate memory for threads\n");
        return 1;
      }

      pthread_create(&(signingthreads[0]), NULL, gpusignmessagethread, NULL);
    }
  }

  /* Run as a CPU node if GPU was not requested or was not initialised successfully */
  if(workertype == 0)
  {
    /* Initialise a CPU based worker node */
    host_log_printf(logfileh, "Info: Initialising node as CPU worker\n");
    workertype = 0;

    /* Check that a sensible number of threads has been requested */
    threadcount = atoi(argv[3]);
    if((threadcount < 1) || (threadcount > 64))
    {
      threadcount = 1;
    }

    /* Create a queue and a thread for each signing instance */
    waitingqueues = (queue **)calloc(threadcount, sizeof(queue *));
    if(waitingqueues == NULL)
    {
      host_log_printf(logfileh, "Error: Unable to allocate memory for threads\n");
      return 1;
    }

    signingthreads = (pthread_t *)calloc(threadcount, sizeof(pthread_t));
    if(signingthreads == NULL)
    {
      host_log_printf(logfileh, "Error: Unable to allocate memory for threads\n");
      return 1;
    }

    for(threadi = 0; threadi < threadcount; threadi++)
    {
      waitingqueues[threadi] = create_queue(logfileh, threadi);
      pthread_create(&(signingthreads[threadi]), NULL, signmessagethread, waitingqueues[threadi]);
    }
  }

  /* Start one thread for communicating with the controller */
  controllerqueue = create_queue(logfileh, 0);
  pthread_create(&srthread, NULL, controllerthread, nodeid);

  while(apprunning)
  {
    sleep(1);
  }

  /* Cleanup CPU resources when we are shutting down */
  for(threadi = 0; threadi < threadcount; threadi++)
  {
    pthread_join(signingthreads[threadi], NULL);
  }
  free(signingthreads);

  pthread_join(srthread, NULL);

  if(workertype == 0)
  {
    for(threadi = 0; threadi < threadcount; threadi++)
    {
      destroy_queue(waitingqueues[threadi]);
    }
    free(waitingqueues);
  }

  if(workertype == 1)
  {
    /* Cleanup GPU resources */
    int i;
    for (i = 0; i < totalstreams; i++)
    {
      free(streams[i].host_message_mem);
      free(streams[i].host_meta_mem);
      cudaSetDevice(streams[i].device);
      cudaStreamDestroy(streams[i].stream);
    }
    free(streams);
  }

  destroy_queue(controllerqueue);

  return 0;
}
