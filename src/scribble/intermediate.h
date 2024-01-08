/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __INTERMEDIATE_H__
#define __INTERMEDIATE_H__

#include <binder.h>
#include <ir.h>

extern IRProgram   generate(BackendConnection *conn, BoundNode *program);
extern IRFunction  evaluate(BoundNode *expr);

#endif /* __INTERMEDIATE_H__ */
