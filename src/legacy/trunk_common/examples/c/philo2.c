#include <stdio.h>
#include <pthread.h>
#include <unistd.h>

/* Mutex */

pthread_mutex_t a_mutex;
pthread_mutex_t b_mutex;

/* Barriers */

pthread_barrier_t wall;

/* The philosophers around the table */

void* diderot (void* name)
{
  printf("Diderot is at table\n");
  //sleep(1);
  pthread_barrier_wait(&wall);
  pthread_mutex_lock(&a_mutex); /* P(a) */
  pthread_mutex_lock(&b_mutex); /* P(b) */
  printf("Diderot has finished his meal\n");
  pthread_mutex_unlock(&b_mutex); /* V(b) */
  pthread_mutex_unlock(&a_mutex); /* V(a) */
  pthread_exit(NULL); /* kind of exit for pthreads ? */
}

void* voltaire (void* name)
{
  printf("Voltaire is at table\n");
  //sleep(1);
  pthread_barrier_wait(&wall);
  pthread_mutex_lock(&b_mutex);
  pthread_mutex_lock(&a_mutex);
  printf("Voltaire has finished his meal\n");
  pthread_mutex_unlock(&a_mutex);
  pthread_mutex_unlock(&b_mutex);
  pthread_exit(NULL);
}

/* Launcher */

main ()
{

  pthread_mutex_init(&a_mutex,NULL);
  pthread_mutex_init(&b_mutex,NULL);
  pthread_barrier_init(&wall,NULL,2);

  pthread_t diderot_thread;
  pthread_t voltaire_thread;

  printf("Let's have a nice dinner.\n");
  pthread_create(&diderot_thread,NULL,diderot,NULL);
  pthread_create(&voltaire_thread,NULL,voltaire,NULL);
  pthread_join(diderot_thread, NULL);
  pthread_join(voltaire_thread, NULL);
  // Can't go futher until both Diderot and Voltaire finish their lunch

  return 0;
}
