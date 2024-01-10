/*
 * Copyright (c) 2024, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <stdlib.h>

#include <options.h>
#include <type.h>

extern int backend_main(StringView path);

int main(int argc, char **argv)
{
    if (argc != 2) {
        exit(1);
    }
    set_option(sv_from("trace"), sv_from("IPC"));
    log_init();
    type_registry_init();
    return backend_main(sv_from(argv[1]));
}
