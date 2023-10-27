/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sys/stat.h>
#include <unistd.h>

#include <fs.h>

size_t fs_file_size(StringView file_name)
{
    struct stat st;
    if (stat(sv_cstr(file_name), &st) == 0) {
        fatal("fs_file_size(%.*s): Cannot stat '%.*s'", SV_ARG(file_name), SV_ARG(file_name));
    }
    return st.st_size;
}

bool fs_file_exists(StringView file_name)
{
    return access(sv_cstr(file_name), F_OK) == 0;
}

bool fs_is_directory(StringView file_name)
{
    struct stat st;
    if (stat(sv_cstr(file_name), &st) == 0) {
        fatal("fs_is_directory(%.*s): Cannot stat '%.*s'", SV_ARG(file_name), SV_ARG(file_name));
    }
    return (st.st_mode & S_IFDIR) != 0;
}

bool fs_is_newer(StringView file_name1, StringView file_name2)
{
    struct stat st1, st2;
    if (stat(sv_cstr(file_name1), &st1) != 0) {
        fatal("fs_is_newer(%.*s, %.*s): Cannot stat '%.*s'", SV_ARG(file_name1), SV_ARG(file_name2), SV_ARG(file_name1));
    }
    if (stat(sv_cstr(file_name2), &st2) != 0) {
        fatal("fs_is_newer(%.*s, %.*s): Cannot stat '%.*s'", SV_ARG(file_name1), SV_ARG(file_name2), SV_ARG(file_name2));
    }
    if (st1.st_mtimespec.tv_sec == st2.st_mtimespec.tv_sec) {
        return st1.st_mtimespec.tv_nsec > st2.st_mtimespec.tv_nsec;
    }
    return st1.st_mtimespec.tv_sec > st2.st_mtimespec.tv_sec;
}
