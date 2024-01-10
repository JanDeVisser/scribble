/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>
#include <json.h>
#include <parser.h>

DA_IMPL(JSONValue);
DA_IMPL(JSONNVPair);

void json_encode_to_builder(JSONValue *value, StringBuilder *sb, int indent)
{
    switch (value->type) {
    case JSON_TYPE_OBJECT: {
        sb_append_cstr(sb, "{\n");
        for (size_t ix = 0; ix < value->object.size; ix++) {
            if (ix > 0) {
                sb_append_cstr(sb, ",\n");
            }
            JSONNVPair *nvp = da_element_JSONNVPair(&value->object, ix);
            sb_printf(sb, "%*s\"%.*s\": ", indent + 4, "", SV_ARG(nvp->name));
            json_encode_to_builder(&nvp->value, sb, indent + 4);
        }
        sb_printf(sb, "\n%*c", indent + 1, '}');
    } break;
    case JSON_TYPE_ARRAY: {
        sb_append_cstr(sb, "[\n");
        for (int ix = 0; ix < value->array.size; ix++) {
            if (ix > 0) {
                sb_append_cstr(sb, ",");
            }
            JSONValue *elem = da_element_JSONValue(&value->array, ix);
            json_encode_to_builder(elem, sb, indent + 4);
        }
        sb_printf(sb, "\n%*c", indent + 1, ']');
    } break;
    case JSON_TYPE_STRING:
        sb_printf(sb, "\"%.*s\"", SV_ARG(value->string));
        break;
    case JSON_TYPE_DOUBLE:
        sb_printf(sb, "%f", value->double_number);
        break;
    case JSON_TYPE_INT:
        sb_printf(sb, "%.*s", SV_ARG(sv_render_integer(value->int_number)));
        break;
    case JSON_TYPE_BOOLEAN:
        sb_append_cstr(sb, (value->boolean) ? "true" : "false");
        break;
    case JSON_TYPE_NULL:
        sb_append_cstr(sb, "null");
        break;
    default:
        UNREACHABLE();
    }
}

JSONValue json_object(void)
{
    JSONValue result = { 0 };
    result.type = JSON_TYPE_OBJECT;
    return result;
}

JSONValue json_array(void)
{
    JSONValue result = { 0 };
    result.type = JSON_TYPE_ARRAY;
    return result;
}

JSONValue json_null(void)
{
    return (JSONValue) { .type = JSON_TYPE_NULL };
}

JSONValue json_string(StringView sv)
{
    return (JSONValue) { .type = JSON_TYPE_STRING, .string = sv };
}

JSONValue json_number(double number)
{
    return (JSONValue) { .type = JSON_TYPE_DOUBLE, .double_number = number };
}

JSONValue json_int(int number)
{
    return (JSONValue) { .type = JSON_TYPE_INT, .int_number = i32(number) };
}

JSONValue json_integer(Integer number)
{
    return (JSONValue) { .type = JSON_TYPE_INT, .int_number = number };
}

JSONValue json_bool(bool value)
{
    return (JSONValue) { .type = JSON_TYPE_BOOLEAN, .boolean = value };
}

void json_append(JSONValue *array, JSONValue elem)
{
    assert(array->type == JSON_TYPE_ARRAY);
    da_append_JSONValue(&array->array, elem);
}

OptionalJSONValue json_at(JSONValue *array, size_t index)
{
    assert(array->type == JSON_TYPE_ARRAY);
    if (index < array->array.size) {
        RETURN_VALUE(JSONValue, *da_element_JSONValue(&array->array, index));
    }
    RETURN_EMPTY(JSONValue);
}

void json_set(JSONValue *obj, char const *attr, JSONValue elem)
{
    json_set_sv(obj, sv_copy_cstr(attr), elem);
}

void json_set_sv(JSONValue *value, StringView attr, JSONValue elem)
{
    assert(value->type == JSON_TYPE_OBJECT);
    JSONNVPair nvp = { .name = attr, .value = elem };
    da_append_JSONNVPair(&value->object, nvp);
}

void json_set_cstr(JSONValue *obj, char const *attr, char const *s)
{
    json_set(obj, attr, json_string(sv_from(s)));
}

void json_set_string(JSONValue *obj, char const *attr, StringView sv)
{
    json_set(obj, attr, json_string(sv));
}

void json_set_int(JSONValue *obj, char const *attr, int i)
{
    json_set(obj, attr, json_int(i));
}

bool json_has(JSONValue *value, char const *attr)
{
    assert(value->type == JSON_TYPE_OBJECT);
    for (size_t ix = 0; ix < value->object.size; ++ix) {
        JSONNVPair const *pair = da_element_JSONNVPair(&value->object, ix);
        if (sv_eq_cstr(pair->name, attr)) {
            return true;
        }
    }
    return false;
}

OptionalJSONValue json_get(JSONValue *obj, char const *attr)
{
    assert(obj->type == JSON_TYPE_OBJECT);
    for (size_t ix = 0; ix < obj->object.size; ++ix) {
        JSONNVPair const *pair = da_element_JSONNVPair(&obj->object, ix);
        if (sv_eq_cstr(pair->name, attr)) {
            RETURN_VALUE(JSONValue, pair->value);
        }
    }
    RETURN_EMPTY(JSONValue);
}

JSONValue json_get_default(JSONValue *obj, char const *attr, JSONValue default_)
{
    OptionalJSONValue ret_maybe = json_get(obj, attr);
    if (ret_maybe.has_value) {
        return ret_maybe.value;
    }
    return default_;
}

bool json_get_bool(JSONValue *obj, char const *attr, bool default_)
{
    JSONValue v = json_get_default(obj, attr, json_bool(default_));
    assert(v.type == JSON_TYPE_BOOLEAN);
    return v.boolean;
}

int json_get_int(JSONValue *obj, char const *attr, int default_)
{
    JSONValue v = json_get_default(obj, attr, json_int(default_));
    assert(v.type == JSON_TYPE_INT);
    Integer as_i32 = MUST_OPTIONAL(Integer, integer_coerce_to(v.int_number, I32));
    return as_i32.i32;
}

StringView json_get_string(JSONValue *obj, char const *attr, StringView default_)
{
    JSONValue v = json_get_default(obj, attr, json_string(default_));
    assert(v.type == JSON_TYPE_STRING);
    return v.string;
}

StringView json_encode(JSONValue value)
{
    StringBuilder sb = { 0 };
    json_encode_to_builder(&value, &sb, 0);
    return sb.view;
}

ErrorOrJSONValue json_decode_value(StringScanner *ss)
{
    ss_skip_whitespace(ss);
    switch (ss_peek(ss)) {
    case 0:
        ERROR(JSONValue, JSONError, 0, "Expected value");
    case '{': {
        JSONValue result = json_object();
        ss_skip_one(ss);
        ss_skip_whitespace(ss);
        while (ss_peek(ss) != '}') {
            if (ss_peek(ss) != '"') {
                ERROR(JSONValue, JSONError, 0, "Expected '\"'");
            }
            JSONValue name = TRY(JSONValue, json_decode_value(ss));
            ss_skip_whitespace(ss);
            if (!ss_expect(ss, ':')) {
                ERROR(JSONValue, JSONError, 0, "Expected ':'");
            }
            ss_skip_whitespace(ss);
            JSONValue value = TRY(JSONValue, json_decode_value(ss));
            json_set_sv(&result, name.string, value);
            ss_skip_whitespace(ss);
            ss_expect(ss, ',');
            ss_skip_whitespace(ss);
        }
        ss_skip_one(ss);
        RETURN(JSONValue, result);
    }
    case '[': {
        JSONValue result = json_array();
        ss_skip_one(ss);
        ss_skip_whitespace(ss);
        while (ss_peek(ss) != ']') {
            JSONValue value = TRY(JSONValue, json_decode_value(ss));
            ss_skip_whitespace(ss);
            json_append(&result, value);
            if (ss_peek(ss) == ',') {
                ss_skip_one(ss);
            }
            ss_skip_whitespace(ss);
        }
        ss_skip_one(ss);
        RETURN(JSONValue, result);
    }
    case '"': {
        ss_skip_one(ss);
        ss_reset(ss);
        while (true) {
            int ch = ss_peek(ss);
            if (ch == '\\') {
                ss_skip(ss, 2);
            } else if (ch == '"') {
                JSONValue ret = json_string(ss_read_from_mark(ss));
                ss_skip(ss, 1);
                RETURN(JSONValue, ret);
            } else if (ch == 0) {
                ERROR(JSONValue, JSONError, 0, "Unterminated string");
            } else {
                ss_skip_one(ss);
            }
        }
    }
    default: {
        if (isdigit(ss_peek(ss))) {
            ss_reset(ss);
            while (isdigit(ss_peek(ss))) {
                ss_skip_one(ss);
            }
            if (ss_peek(ss) == '.') {
                ss_skip_one(ss);
                while (isdigit(ss_peek(ss))) {
                    ss_skip_one(ss);
                }
                ERROR(JSONValue, JSONError, 0, "Can't parse doubles in JSON yet");
            }
            StringView         sv = ss_read_from_mark(ss);
            IntegerParseResult parse_result = sv_parse_integer(sv, I64);
            if (parse_result.success) {
                RETURN(JSONValue, json_integer(parse_result.integer));
            }
            parse_result = sv_parse_integer(sv, U64);
            if (!parse_result.success) {
                ERROR(JSONValue, JSONError, 0, "Unparseable integer %.*s", SV_ARG(sv));
            }
            RETURN(JSONValue, json_integer(parse_result.integer));
        }
        if (ss_expect_sv(ss, sv_from("true"))) {
            RETURN(JSONValue, json_bool(true));
        }
        if (ss_expect_sv(ss, sv_from("false"))) {
            RETURN(JSONValue, json_bool(false));
        }
        if (ss_expect_sv(ss, sv_from("null"))) {
            RETURN(JSONValue, json_null());
        }
        ERROR(JSONValue, JSONError, 0, "Invalid JSON");
    }
    }
}

ErrorOrJSONValue json_decode(StringView json)
{
    StringScanner ss = ss_create(json);
    return json_decode_value(&ss);
}

#ifdef JSON_TEST

int main(int argc, char **argv)
{
    JSONValue obj = json_object();
    json_set(&obj, "foo", json_string(sv_from("bar")));
    json_set(&obj, "quux", json_string(sv_from("hello")));
    StringView json = json_encode(obj);
    printf("%.*s\n", SV_ARG(json));

    ErrorOrJSONValue decoded = json_decode(json);
    if (ErrorOrJSONValue_is_error(decoded)) {
        printf("Decoding error: %s\n", Error_to_string(decoded.error));
    } else {
        json = json_encode(decoded.value);
        printf("%.*s\n", SV_ARG(json));
    }
    return 0;
}

#endif
