/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __ARCH_ARM64_OPERATOR_H__
#define __ARCH_ARM64_OPERATOR_H__

ValueLocation arm64operator_apply(ARM64Function *function, type_id lhs_type, Operator op, type_id rhs_type, ValueLocation *result);

#endif /* __ARCH_ARM64_OPERATOR_H__ */
