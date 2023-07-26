/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include "binder.h"
#include "parser.h"

#ifndef __GRAPH_H__
#define __GRAPH_H__

extern void graph_program(SyntaxNode *program);
extern void graph_ast(BoundNode *program);

#endif /* __GRAPH_H__ */
