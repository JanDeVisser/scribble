/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <error.h>

#ifndef __IO_H__
#define __IO_H__

ErrorOrChar read_file_by_name(char const* file_name);
ErrorOrChar read_file_at(int dir_fd, char const* file_name);
ErrorOrChar read_file(int fd);

#endif /* __IO_H__ */
