#include <pthread.h>
#include <stdio.h>

int x = 5;
pthread_mutex_t m;

void *increment(void *arg)
{
  int i;
  /* for (i=0; i<5; i++) */
  /* { */
  pthread_mutex_lock(&m);
  x++;
  pthread_mutex_unlock(&m);
  /* } */
  pthread_exit(NULL);
}

void main()
{
  pthread_mutex_init(&m, NULL);

  pthread_t t1, t2;
  pthread_create(&t1, NULL, increment, NULL);
  pthread_create(&t2, NULL, increment, NULL);

  char c = 'c';
  printf("%c%d\n",c,x);
  pthread_exit(NULL);
}
