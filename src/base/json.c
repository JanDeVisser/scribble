/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>
#include <json.h>

DA_IMPL(JSONValue);
DA_IMPL(JSONNVPair);

void json_encode_to_builder(JSONValue *value, StringBuilder *sb, int indent)
{
    assert(value->type != JSON_TYPE_ERROR);
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
    case JSON_TYPE_NUMBER:
        sb_printf(sb, "%f", value->number);
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

JSONValue json_error(StringView msg)
{
    return (JSONValue) { .type = JSON_TYPE_ERROR, .string = msg };
}

JSONValue json_number(double number)
{
    return (JSONValue) { .type = JSON_TYPE_NUMBER, .number = number };
}

JSONValue json_bool(bool value)
{
    return (JSONValue) { .type = JSON_TYPE_BOOLEAN, .boolean = value };
}

void json_append(JSONValue *value, JSONValue elem)
{
    assert(value->type == JSON_TYPE_ARRAY);
    da_append_JSONValue(&value->array, elem);
}

void json_set(JSONValue *value, StringView attr, JSONValue elem)
{
    assert(value->type == JSON_TYPE_OBJECT);
    JSONNVPair nvp = { .name = attr, .value = elem };
    da_append_JSONNVPair(&value->object, nvp);
}

StringView json_encode(JSONValue value)
{
    StringBuilder sb = { 0 };
    json_encode_to_builder(&value, &sb, 0);
    return sb.view;
}

JSONValue json_decode_value(StringScanner *ss)
{
    ss_skip_whitespace(ss);
    switch (ss_peek(ss)) {
    case 0:
        return json_error(sv_from("Expected value"));
    case '{': {
        JSONValue result = json_object();
        ss_skip_one(ss);
        ss_skip_whitespace(ss);
        while (ss_peek(ss) != '}') {
            int ch = ss_peek(ss);
            if (ss_peek(ss) != '"') {
                return json_error(sv_from("Expected '\"'"));
            }
            JSONValue name = json_decode_value(ss);
            if (name.type == JSON_TYPE_ERROR) {
                return name;
            }
            ss_skip_whitespace(ss);
            if (!ss_expect(ss, ':')) {
                return json_error(sv_from("Expected ':'"));
            }
            ss_skip_whitespace(ss);
            JSONValue value = json_decode_value(ss);
            if (value.type == JSON_TYPE_ERROR) {
                return value;
            }
            json_set(&result, name.string, value);
            ss_skip_whitespace(ss);
            ss_expect(ss, ',');
            ss_skip_whitespace(ss);
        }
        ss_skip_one(ss);
        return result;
    }
    case '[': {
        JSONValue result = json_array();
        ss_skip_one(ss);
        ss_skip_whitespace(ss);
        while (ss_peek(ss) != ']') {
            JSONValue value = json_decode_value(ss);
            ss_skip_whitespace(ss);
            json_append(&result, value);
            if (ss_peek(ss) == ',') {
                ss_skip_one(ss);
            }
            ss_skip_whitespace(ss);
        }
        ss_skip_one(ss);
        return result;
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
                return ret;
            } else if (ch == 0) {
                return json_error(sv_from("Unterminated string"));
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
                NYI("Can't parse doubles in JSON yet");
            }
            StringView sv = ss_read_from_mark(ss);
            IntegerParseResult parse_result = sv_parse_integer(sv, I64);
            return json_number((double) parse_result.integer.i64);
        } else if (ss_expect_sv(ss, sv_from("true"))) {
            return json_bool(true);
        } else if (ss_expect_sv(ss, sv_from("false"))) {
            return json_bool(false);
        } else if (ss_expect_sv(ss, sv_from("null"))) {
            return json_null();
        } else {
            return json_error(sv_from("Invalid JSON"));
        }
    }
    }
}

JSONValue json_decode(StringView json)
{
    StringScanner ss = ss_create(json);
    return json_decode_value(&ss);
}

#ifdef JSON_TEST

int main(int argc, char **argv)
{
    JSONValue obj = json_object();
    json_set(&obj, sv_from("foo"), json_string(sv_from("bar")));
    json_set(&obj, sv_from("quux"), json_string(sv_from("hello")));
    StringView json = json_encode(obj);
    printf("%.*s\n", SV_ARG(json));

    JSONValue decoded = json_decode(json);
    if (decoded.type == JSON_TYPE_ERROR) {
        printf("Decoding error: %.*s\n", SV_ARG(decoded.string));
    }
    json = json_encode(decoded);
    printf("%.*s\n", SV_ARG(json));
    return 0;
}

#endif
