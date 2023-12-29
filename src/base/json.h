/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <sv.h>

#ifndef __JSON_H__
#define __JSON_H__

typedef enum {
    JSON_TYPE_ERROR = 0,
    JSON_TYPE_OBJECT,
    JSON_TYPE_ARRAY,
    JSON_TYPE_STRING,
    JSON_TYPE_NUMBER,
    JSON_TYPE_BOOLEAN,
    JSON_TYPE_NULL,
} JSONType;

DA_VOID_WITH_NAME(JSONValue, JSONValues);
DA_VOID_WITH_NAME(JSONNVPair, JSONNVPairs);

typedef struct json_value {
    JSONType type;
    union {
        JSONNVPairs object;
        JSONValues  array;
        StringView  string;
        double      number;
        bool        boolean;
    };
} JSONValue;

typedef struct {
    StringView name;
    JSONValue  value;
} JSONNVPair;

DA_FUNCTIONS(JSONValue);
DA_FUNCTIONS(JSONNVPair);

extern JSONValue  json_object(void);
extern JSONValue  json_array(void);
extern JSONValue  json_null(void);
extern JSONValue  json_string(StringView sv);
extern JSONValue  json_error(StringView msg);
extern JSONValue  json_number(double number);
extern JSONValue  json_bool(bool value);
extern void       json_append(JSONValue *value, JSONValue elem);
extern void       json_set(JSONValue *value, StringView attr, JSONValue elem);
extern StringView json_encode(JSONValue value);
extern JSONValue  json_decode(StringView json_text);

#endif /* __JSON_H__ */
