/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define HAVE_PTHREAD_H

#ifndef __MUTEX_H__
#define __MUTEX_H__

#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif /* HAVE_PTHREAD_H */
#ifdef HAVE_INITIALIZECRITICALSECTION
#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif /* HAVE_WINDOWS_H */
#endif /* HAVE_INITIALIZECRITICALSECTION */

#include <sv.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct mutex {
    StringView name;
#ifdef HAVE_PTHREAD_H
    pthread_mutex_t *mutex;
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    CRITICAL_SECTION   cs;
#endif /* HAVE_PTHREAD_H */
} Mutex;

typedef struct condition {
    Mutex mutex;
    bool  borrowed_mutex;
#ifdef HAVE_PTHREAD_H
    pthread_cond_t *condition;
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    CONDITION_VARIABLE condition;
#endif
} Condition;

Mutex mutex_create(void);
void  mutex_free(Mutex mutex);
Mutex mutex_create_withname(StringView name);
void  mutex_lock(Mutex mutex);
int   mutex_try_lock(Mutex mutex);
void  mutex_unlock(Mutex mutex);

Condition condition_create();
Condition condition_create_with_borrowed_mutex(Mutex mutex);
void      condition_acquire(Condition);
int       condition_try_acquire(Condition);
void      condition_release(Condition);
void      condition_wakeup(Condition);
void      condition_sleep(Condition);

#ifdef __cplusplus
}
#endif

#endif /* __MUTEX_H__ */
