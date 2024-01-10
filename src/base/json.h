/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#ifndef __JSON_H__
#define __JSON_H__

#include <integer.h>
#include <sv.h>

typedef enum {
    JSON_TYPE_NULL = 0,
    JSON_TYPE_OBJECT,
    JSON_TYPE_ARRAY,
    JSON_TYPE_STRING,
    JSON_TYPE_INT,
    JSON_TYPE_DOUBLE,
    JSON_TYPE_BOOLEAN,
} JSONType;

DA_VOID_WITH_NAME(JSONValue, JSONValues);
DA_VOID_WITH_NAME(JSONNVPair, JSONNVPairs);

typedef struct json_value {
    JSONType type;
    union {
        JSONNVPairs object;
        JSONValues  array;
        StringView  string;
        Integer     int_number;
        double      double_number;
        bool        boolean;
    };
} JSONValue;

typedef struct {
    StringView name;
    JSONValue  value;
} JSONNVPair;

DA_FUNCTIONS(JSONValue)
DA_FUNCTIONS(JSONNVPair)
OPTIONAL(JSONValue)
ERROR_OR(JSONValue)

extern JSONValue         json_object(void);
extern JSONValue         json_array(void);
extern JSONValue         json_null(void);
extern JSONValue         json_string(StringView sv);
extern JSONValue         json_number(double number);
extern JSONValue         json_int(int number);
extern JSONValue         json_integer(Integer number);
extern JSONValue         json_bool(bool value);
extern void              json_append(JSONValue *array, JSONValue elem);
extern OptionalJSONValue json_at(JSONValue *array, size_t index);
extern size_t            json_len(JSONValue *array);
extern void              json_set(JSONValue *obj, char const *attr, JSONValue elem);
extern void              json_set_string(JSONValue *obj, char const *attr, StringView sv);
extern void              json_set_cstr(JSONValue *obj, char const *attr, char const *s);
void                     json_set_int(JSONValue *obj, char const *attr, int i);
extern void              json_set_sv(JSONValue *obj, StringView attr, JSONValue elem);
extern OptionalJSONValue json_get(JSONValue *obj, char const *attr);
extern JSONValue         json_get_default(JSONValue *obj, char const *attr, JSONValue default_);
extern bool              json_get_bool(JSONValue *obj, char const *attr, bool default_);
extern int               json_get_int(JSONValue *obj, char const *attr, int default_);
extern StringView        json_get_string(JSONValue *obj, char const *attr, StringView default_);
extern bool              json_has(JSONValue *obj, char const *attr);
extern StringView        json_encode(JSONValue value);
extern ErrorOrJSONValue  json_decode(StringView json_text);

#endif /* __JSON_H__ */
