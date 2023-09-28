/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#define HAVE_PTHREAD_H

#ifndef __THREADONCE_H__
#define __THREADONCE_H__

#ifdef HAVE_PTHREAD_H

#include <pthread.h>

#define THREAD_ONCE(var) static pthread_once_t var = PTHREAD_ONCE_INIT
#define ONCE(var, fnc) pthread_once(&var, fnc);

#elif defined(HAVE_INITONCEEXECUTEONCE)

#include <windows.h>

OBLCORE_IMPEXP BOOL CALLBACK InitHandleFunction(PINIT_ONCE, PVOID, PVOID *);
#define THREAD_ONCE(var) static INIT_ONCE var = INIT_ONCE_STATIC_INIT;
#define ONCE(var, fnc) (void) InitOnceExecuteOnce(&var, InitHandleFunction, (fnc), NULL);

#else

#error "Please provide threadonce implementation"

#endif

#endif /* __THREADONCE_H__ */
