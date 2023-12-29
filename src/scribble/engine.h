/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <da.h>
#include <ir.h>
#include <type.h>

#ifndef __ENGINE_H__
#define __ENGINE_H__

typedef enum execution_mode {
    EM_RUN = 0x00,
    EM_SINGLE_STEP = 0x01,
    EM_STEP_OVER = 0x02,
    EM_RUN_TO_RETURN = 0x04,
    EM_CONTINUE = 0x08,
} ExecutionMode;

typedef enum {
    EMT_STAGE_INIT,
    EMT_STAGE_START,
    EMT_PROGRAM_START,
    EMT_FUNCTION_ENTRY,
    EMT_ON_INSTRUCTION,
    EMT_AFTER_INSTRUCTION,
    EMT_FUNCTION_RETURN,
    EMT_PROGRAM_EXIT,
    EMT_STAGE_END,
} ExecutionMessageType;

typedef enum {
    ES_INIT,
    ES_PARSE,
    ES_BIND,
    ES_INTERMEDIATE,
    ES_GENERATE,
    ES_EXECUTE,
    ES_INTERPRET,
    ES_MAX,
} EngineStage;

typedef struct ExecutionMessage {
    ExecutionMessageType type;
    void                *payload;
} ExecutionMessage;

typedef struct command {
    StringView command;
    StringList arguments;
    StringView request;
} Command;

typedef struct {
    bool debug;
    bool execute;
} EngineStageConfig;

typedef struct engine_connection {
    int               fd;
    EngineStage       stage;
    EngineStageConfig config[ES_MAX];
    StringView        target;
    void             *context;
} EngineConnection;

typedef bool (*EngineStageExecutor)(EngineConnection *, ExecutionMessage);

extern void engine_start_stage(EngineConnection *conn, EngineStage stage);
extern bool engine_callback(EngineConnection *conn, ExecutionMessage msg);

#endif /* __ENGINE_H__ */
