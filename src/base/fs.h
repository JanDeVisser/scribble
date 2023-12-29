/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <error_or.h>
#include <sv.h>

#ifndef __FS_H__
#define __FS_H__

extern size_t     fs_file_size(StringView file_name);
extern bool       fs_file_exists(StringView file_name);
extern bool       fs_is_directory(StringView file_name);
extern bool       fs_is_newer(StringView file_name1, StringView file_name2);
extern ErrorOrInt fs_unlink(StringView file_name);

#endif /* __FS_H__ */
