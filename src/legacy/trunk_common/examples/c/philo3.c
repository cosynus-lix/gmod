#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
pthread_mutex_t mutex1;
pthread_mutex_t mutex2;
pthread_mutex_t mutex3;
pthread_barrier_t wall;
int accu = 0;

void* circle1 (void* name)
{
  pthread_barrier_wait(&wall);
  /* while(0 == 0) */
    {
      pthread_mutex_lock(&mutex1);
      printf("[thread 1] has locked mutex1\n");
      pthread_mutex_lock(&mutex2);
      printf("[thread 1] has locked mutex2\n");
      printf("turn = %i [1]\n",++accu);
      pthread_mutex_unlock(&mutex2);
      printf("[thread 1] has unlocked mutex2\n");
      pthread_mutex_unlock(&mutex1);
      printf("[thread 1] has unlocked mutex1\n");
      printf("\n");
    }
  pthread_exit(NULL);
}

void* circle2 (void* name)
{
  pthread_barrier_wait(&wall);
  /* while(0 == 0) */
    {
      pthread_mutex_lock(&mutex2);
      printf("[thread 2] has locked mutex2\n");
      pthread_mutex_lock(&mutex3);
      printf("[thread 2] has locked mutex3\n");
      printf("turn = %i [2]\n",++accu);
      pthread_mutex_unlock(&mutex3);
      printf("[thread 2] has unlocked mutex3\n");
      pthread_mutex_unlock(&mutex2);
      printf("[thread 2] has unlocked mutex2\n");
      printf("\n");
    }
  pthread_exit(NULL);
}

void* circle3 (void* name)
{
  pthread_barrier_wait(&wall);
  /* while(0 == 0) */
    {
      pthread_mutex_lock(&mutex3);
      printf("[thread 3] has locked mutex3\n");
      pthread_mutex_lock(&mutex1);
      printf("[thread 3] has locked mutex1\n");
      printf("turn = %i [3]\n",++accu);
      pthread_mutex_unlock(&mutex1);
      printf("[thread 3] has unlocked mutex1\n");
      pthread_mutex_unlock(&mutex3);
      printf("[thread 3] has unlocked mutex3\n");
      printf("\n");
    }
  pthread_exit(NULL);
}

main()
{
  pthread_mutex_init(&mutex1,NULL);
  pthread_mutex_init(&mutex2,NULL);
  pthread_mutex_init(&mutex3,NULL);
  pthread_barrier_init (&wall,NULL,3);
  pthread_t thr1;
  pthread_t thr2;
  pthread_t thr3;
  pthread_create(&thr1,NULL,circle1,NULL);
  pthread_create(&thr2,NULL,circle2,NULL);
  pthread_create(&thr3,NULL,circle3,NULL);
  pthread_join(thr1,NULL);
  pthread_join(thr2,NULL);
  pthread_join(thr3,NULL);
  return 0;
}
