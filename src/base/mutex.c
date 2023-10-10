/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define HAVE_PTHREAD_H

#include <errno.h>

#define STATIC_ALLOCATOR
#define ALLOCATOR_SLAB_SZ 1024
#include <allocate.h>
#include <log.h>
#include <mutex.h>

Mutex mutex_create(void)
{
    return mutex_create_withname(sv_null());
}

void mutex_free(Mutex mutex)
{
#ifdef HAVE_PTHREAD_H
    pthread_mutex_destroy(mutex.mutex);
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    DeleteCriticalSection(&(mutex->cs));
#endif /* HAVE_PTHREAD_H */
}

Mutex mutex_create_withname(StringView name)
{
    Mutex mutex;

    mutex.name = name;
#ifdef HAVE_PTHREAD_H
    pthread_mutexattr_t attr;
#endif /* HAVE_PTHREAD_H */

#ifdef HAVE_PTHREAD_H
    pthread_mutexattr_init(&attr);
    pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
    mutex.mutex = allocate_new(pthread_mutex_t);
    if ((errno = pthread_mutex_init(mutex.mutex, &attr))) {
        fatal("Error creating mutex: %s", strerror(errno));
    }
    pthread_mutexattr_destroy(&attr);
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    InitializeCriticalSection(&(mutex->cs));
#endif /* HAVE_PTHREAD_H */

    return mutex;
}

void mutex_lock(Mutex mutex)
{
    int retval = 0;

    trace(CAT_LIB, "Locking mutex");
#ifdef HAVE_PTHREAD_H
    errno = pthread_mutex_lock(mutex.mutex);
    if (errno) {
        retval = -1;
    }
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    EnterCriticalSection(&(mutex->cs));
#endif /* HAVE_PTHREAD_H */
    if (retval) {
        fatal("Error locking mutex: %d", errno);
    }
    trace(CAT_LIB, "Mutex locked");
}

/**
 * @return 0 if the mutex was successfully locked
 *         1 if the mutex was owned by another thread
 *         -1 If an error occurred.
 */
int mutex_try_lock(Mutex mutex)
{
    int retval;

    trace(CAT_LIB, "Trying to lock mutex");
#ifdef HAVE_PTHREAD_H
    errno = pthread_mutex_trylock(mutex.mutex);
    switch (errno) {
    case 0:
        retval = 0;
        break;
    case EBUSY:
        retval = 1;
        break;
    default:
        retval = -1;
        break;
    }
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    retval = (!TryEnterCriticalSection(&mutex->cs)) ? 0 : 1;
#endif /* HAVE_PTHREAD_H */
    trace(CAT_LIB, "Trylock mutex: %s", (retval) ? "Fail" : "Success");
    return retval;
}

void mutex_unlock(Mutex mutex)
{
    int retval = 0;

    trace(CAT_LIB, "Unlocking mutex");
#ifdef HAVE_PTHREAD_H
    errno = pthread_mutex_unlock(mutex.mutex);
    if (errno) {
        retval = -1;
    }
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    EnterCriticalSection(&mutex->cs);
#endif /* HAVE_PTHREAD_H */
    if (retval) {
        fatal("Error unlocking mutex: %d", errno);
    }
    trace(CAT_LIB, "Mutex unlocked");
}

/* ------------------------------------------------------------------------ */
/* -- C O N D I T I O N _ T ----------------------------------------------- */
/* ------------------------------------------------------------------------ */

void condition_free(Condition condition)
{
#ifdef HAVE_PTHREAD_H
    pthread_cond_destroy(condition.condition);
#endif /* HAVE_PTHREAD_H */
    if (!condition.borrowed_mutex) {
        mutex_free(condition.mutex);
    }
}

Condition condition_create_with_borrowed_mutex(Mutex mutex)
{
    Condition condition = { 0 };
    condition.mutex = mutex;
    condition.borrowed_mutex = true;
#ifdef HAVE_PTHREAD_H
    condition.condition = allocate_new(pthread_cond_t);
    pthread_cond_init(condition.condition, NULL);
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    InitializeConditionVariable(&condition->condition);
#endif /* HAVE_PTHREAD_H */
    trace(CAT_LIB, "Condition created");
    return condition;
}

void condition_acquire(Condition condition)
{
    trace(CAT_LIB, "Acquiring condition");
    mutex_lock(condition.mutex);
}

void condition_release(Condition condition)
{
    trace(CAT_LIB, "Releasing condition");
    mutex_unlock(condition.mutex);
}

/**
 * @return 0 if the condition was successfully locked
 *         1 if the condition was owned by another thread
 *         -1 If an error occurred.
 */
int condition_try_acquire(Condition condition)
{
    trace(CAT_LIB, "Trying to acquire condition");
    return mutex_try_lock(condition.mutex);
}

void condition_wakeup(Condition condition)
{
    int retval = 0;

    trace(CAT_LIB, "Waking up condition");
#ifdef HAVE_PTHREAD_H
    errno = pthread_cond_signal(condition.condition);
    if (errno) {
        retval = -1;
    }
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    WakeConditionVariable(&condition->condition);
#endif /* HAVE_PTHREAD_H */
    mutex_unlock(condition.mutex);
    if (retval) {
        fatal("Error waking condition: %d", errno);
    }
    trace(CAT_LIB, "Condition woken up");
}

void condition_sleep(Condition condition)
{
    int retval = 0;

    trace(CAT_LIB, "Going to sleep on condition");
#ifdef HAVE_PTHREAD_H
    errno = pthread_cond_wait(condition.condition, condition.mutex.mutex);
    if (errno) {
        retval = -1;
    }
#elif defined(HAVE_INITIALIZECRITICALSECTION)
    SleepConditionVariableCS(&condition->condition, &condition->mutex->cs, INFINITE);
#endif /* HAVE_PTHREAD_H */
    if (retval) {
        fatal("Error sleeping on condition: %d", errno);
    }
    trace(CAT_LIB, "Woke up from condition");
}

/* ------------------------------------------------------------------------ */
