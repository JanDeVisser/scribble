/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <binder.h>
#include <ir.h>

#ifndef __INTERMEDIATE_H__
#define __INTERMEDIATE_H__

extern IRProgram   generate(BoundNode *program);
extern IRFunction  evaluate(BoundNode *expr);

#endif /* __INTERMEDIATE_H__ */
