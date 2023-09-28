/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

#include <io.h>
#include <mem.h>

ErrorOrChar read_file_by_name(char const *file_name)
{
    int fd = open(file_name, O_RDONLY);
    if (fd < 0) {
        ERROR(Char, IOError, errno, "Could not open file");
    }
    ErrorOrChar ret = read_file(fd);
    close(fd);
    return ret;
}

ErrorOrChar read_file_at(int dir_fd, char const *file_name)
{
    int fd = openat(dir_fd, file_name, O_RDONLY);
    if (fd < 0) {
        ERROR(Char, IOError, errno, "Could not open file");
    }
    ErrorOrChar ret = read_file(fd);
    close(fd);
    return ret;
}

ErrorOrChar read_file(int fd)
{
    struct stat sb;
    if (fstat(fd, &sb) < 0) {
        ERROR(Char, IOError, errno, "Could not fstat file");
    }
    size_t sz = sb.st_size;
    char  *buffer = mem_allocate(sz + 1);
    if (!buffer) {
        ERROR(Char, OutOfMemory, errno, "Out-of-memory allocating file buffer");
    }
    buffer[sz] = 0;
    size_t read_chars = read(fd, buffer, sz);
    if (read_chars < sz) {
        ERROR(Char, IOError, errno, "Could not read file");
    }
    RETURN(Char, buffer);
}
