/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sv.h>

#ifndef __FN_H__
#define __FN_H__

StringView fn_barename(StringView path);
StringView fn_basename(StringView path);
StringView fn_dirname(StringView path);
StringView fn_extension(StringView path);
StringList fn_split_path(StringView path);

#endif /* __FN_H__ */
