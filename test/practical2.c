
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "cond.c"


int pnum;  // number updated when producer runs.
int csum;  // sum computed using pnum when consumer runs.
int p;

int (*pred)(int); // predicate indicating if pnum is to be consumed

//initialise a mutex
pthread_mutex_t mutexp = PTHREAD_MUTEX_INITIALIZER ; // protects pnum
pthread_mutex_t mutexs = PTHREAD_MUTEX_INITIALIZER ;

//initialise thread condition variable
pthread_cond_t condm = PTHREAD_COND_INITIALIZER;



//==============================================
int produceT() {

  scanf("%d",&pnum); // read a number from stdin

  return pnum;
}
//==============================================



//==============================================
void *Produce(void *a) {
  int p;

  p=1;
  pthread_mutex_lock(&mutexp); //locks pnum
  //pthread_mutex_unlock(&mutexs);
  while (p) {
    
    printf("@P-READY\n");
    p = produceT(); //p is pnum
    printf("@PRODUCED %d\n",p);
    pthread_cond_wait(&condm, &mutexp); //waits for signal
    
  }

  //pthread_mutex_unlock(&mutexp);//unlocks pnum

  printf("@P-EXIT\n");
  pthread_exit(NULL);
}
//=============================================



//=============================================
int consumeT() {

  if ( pred(pnum) ) { csum += pnum; }

  return pnum;
}
//=============================================



//=============================================
void *Consume(void *a) {
  int p;

  p=1;

  while (p) { 
    
    pthread_mutex_lock(&mutexp); //locks pnum
    if(pnum != 999){
	    printf("@C-READY\n");
	    p = consumeT();
	    printf("@CONSUMED %d\n",csum);
    }
    pthread_cond_signal(&condm);

    pthread_mutex_unlock(&mutexp);//unlocks pnum
  }

  printf("@C-EXIT\n");
  pthread_exit(NULL);
}
//============================================



int main (int argc, const char * argv[]) {
  // the current number predicate
  static pthread_t prod,cons;//these are the 2 different threads
	long rc;

  //decides which condition function to use. Dont change
  pred = &cond1;
  if (argc>1) {
    if      (!strncmp(argv[1],"2",10)) { pred = &cond2; }
    else if (!strncmp(argv[1],"3",10)) { pred = &cond3; }
  }

  //initialises variables, dont change
  pnum = 999;
  csum=0;
  srand(time(0));
  p=1;


  //creates the producer thread
  printf("@P-CREATE\n");
 	rc = pthread_create(&prod,NULL,Produce,(void *)0);
	if (rc) {
			printf("@P-ERROR %ld\n",rc);
			exit(-1);
		}

  //creates the consumer thread
  printf("@C-CREATE\n");
 	rc = pthread_create(&cons,NULL,Consume,(void *)0);
	if (rc) {
			printf("@C-ERROR %ld\n",rc);
			exit(-1);
		}

  //joins the threads
  printf("@P-JOIN\n");
  pthread_join( prod, NULL);
  printf("@C-JOIN\n");
  pthread_join( cons, NULL);

  //prints the final value
  printf("@CSUM=%d.\n",csum);

  return 0;
}
