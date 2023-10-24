/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <arm64.h>

void arm64scope_set_depth(ARM64Scope *scope, int64_t depth)
{
    scope->cumulative_depth = depth + scope->depth;
    for (ARM64Scope *child = scope->scopes; child; child = child->next) {
        arm64scope_set_depth(child, scope->cumulative_depth);
    }
}
