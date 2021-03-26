
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

// We just have one critical variable: pnum
pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

int isPsTurn ; // true, if the producer needs to produce
// producer waits until consumer has read previous value
pthread_cond_t condP = PTHREAD_COND_INITIALIZER;

// consumer waits until producer has produced new value.
pthread_cond_t condC = PTHREAD_COND_INITIALIZER;

int (*pred)(int); // predicate indicating if pnum is to be consumed

int produceT() {
  printf("P wants mutex\n");
  pthread_mutex_lock(&mutex);
  printf("P has mutex\n");
  printf("P waits for condP\n");
  while( !isPsTurn) {
    pthread_cond_wait(&condP,&mutex);
  }
  // isPsTurn is true
  printf("P done waiting\n");
  scanf("%d",&pnum); // read a number from stdin
  isPsTurn = 0;
  pthread_cond_signal(&condC); // tell consumer it can go...
  printf("P signalled condC\n");
  pthread_mutex_unlock(&mutex);
  printf("P done with mutex\n");
  return pnum;
}

void *Produce(void *a) {
  int p;

  p=1;
  while (p) {
    printf("@P-READY\n");
    p = produceT();
    printf("@PRODUCED %d\n",p);
  }
  printf("@P-EXIT\n");
  pthread_exit(NULL);
}


int consumeT() {
  printf("C wants mutex\n");
  pthread_mutex_lock(&mutex);
  printf("C has mutex\n");
  printf("C waits for condC\n");
  if( isPsTurn ) {
    pthread_cond_wait(&condC,&mutex);
  }
  // isPsTurn == false
  printf("C done waiting\n");
  if ( pred(pnum) ) { csum += pnum; }
  isPsTurn = 1;
  pthread_cond_signal(&condP); // tell producer it can go...
  printf("C signalled condP\n");
  pthread_mutex_unlock(&mutex);
  printf("C done with mutex\n");
  return pnum;
}

void *Consume(void *a) {
  int p;

  p=1;
  while (p) {
      printf("@C-READY\n");
    p = consumeT();
    printf("@CONSUMED %d\n",csum);
  }
  printf("@C-EXIT\n");
  pthread_exit(NULL);
}


int main (int argc, const char * argv[]) {
  // the current number predicate
  static pthread_t prod,cons;
	long rc;

  pred = &cond1;
  if (argc>1) {
    if      (!strncmp(argv[1],"2",10)) { pred = &cond2; }
    else if (!strncmp(argv[1],"3",10)) { pred = &cond3; }
  }


  pnum = 999;
  csum=0;
  srand(time(0));

  rc = pthread_cond_signal(&condP);
	if (rc) {
			printf("SIGNAL-COND-ERROR %ld\n",rc);
		}

  isPsTurn = 1;

  printf("@P-CREATE\n");
 	rc = pthread_create(&prod,NULL,Produce,(void *)0);
	if (rc) {
			printf("@P-ERROR %ld\n",rc);
			exit(-1);
		}
  printf("@C-CREATE\n");
 	rc = pthread_create(&cons,NULL,Consume,(void *)0);
	if (rc) {
			printf("@C-ERROR %ld\n",rc);
			exit(-1);
		}

  printf("@P-JOIN\n");
  pthread_join( prod, NULL);
  printf("@C-JOIN\n");
  pthread_join( cons, NULL);


  printf("@CSUM=%d.\n",csum);

  return 0;
}
