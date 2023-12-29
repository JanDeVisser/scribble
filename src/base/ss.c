/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#include <sv.h>

StringScanner ss_create(StringView sv)
{
    StringScanner ret = { 0 };
    ret.string = sv;
    return ret;
}

void ss_rewind(StringScanner *ss)
{
    ss->point = ss->mark;
}

void ss_reset(StringScanner *ss)
{
    ss->mark = ss->point;
}

void ss_partial_rewind(StringScanner *ss, size_t num)
{
    if (num > (ss->point - ss->mark)) {
        num = ss->point - ss->mark;
    }
    ss->point -= num;
}

void ss_pushback(StringScanner *ss)
{
    if (ss->point > ss->mark) {
        ss->point--;
    }
}

StringView ss_read(StringScanner *ss, size_t num)
{
    if ((int64_t) (num) < 0) {
        num = 0;
    }
    if ((ss->point + num) > ss->string.length) {
        num = ss->string.length - ss->point;
    }
    StringView ret = sv_substring(ss->string, ss->point, num);
    ss->point = ss->point + num;
    return ret;
}

StringView ss_read_from_mark(StringScanner *ss)
{
    size_t num = ss->point - ss->mark;
    if (num > 0) {
        ss_rewind(ss);
        return ss_read(ss, num);
    }
    return sv_null();
}

int ss_readchar(StringScanner *ss)
{
    return (ss->point < ss->string.length - 1) ? ss->string.ptr[++ss->point] : '\0';
}

int ss_peek(StringScanner *ss)
{
    return ss_peek_with_offset(ss, 0);
}

int ss_peek_with_offset(StringScanner *ss, size_t offset)
{
    return ((ss->point + offset) < ss->string.length) ? ss->string.ptr[ss->point + offset] : 0;
}

void ss_skip(StringScanner *ss, size_t num)
{
    if (ss->point + num > ss->string.length) {
        num = ss->string.length - ss->point;
    }
    ss->point += num;
}

void ss_skip_one(StringScanner *ss)
{
    ss_skip(ss, 1);
}

void ss_skip_whitespace(StringScanner *ss)
{
    while (isspace(ss_peek(ss))) {
        ss_skip(ss, 1);
    }
}

bool ss_expect_with_offset(StringScanner *ss, char ch, size_t offset)
{
    if (ss_peek_with_offset(ss, offset) != ch) {
        return false;
    }
    ss->point += offset + 1;
    return true;
}

bool ss_expect(StringScanner *ss, char ch)
{
    return ss_expect_with_offset(ss, ch, 0);
}

bool ss_expect_sv(StringScanner *ss, StringView sv)
{
    if (ss->point + sv.length > ss->string.length) {
        return false;
    }
    if (!sv_eq(sv_substring(ss->string, ss->point, sv.length), sv)) {
        return false;
    }
    ss->point += sv.length;
    return true;
}

bool ss_is_one_of_with_offset(StringScanner *ss, char const *expect, size_t offset)
{
    return strchr(expect, ss_peek_with_offset(ss, offset)) != NULL;
}

bool ss_is_one_of(StringScanner *ss, char const *expect)
{
    return ss_is_one_of_with_offset(ss, expect, 0);
}

bool ss_expect_one_of_with_offset(StringScanner *ss, char const *expect, size_t offset)
{
    if (ss_is_one_of_with_offset(ss, expect, offset)) {
        ss->point += offset + 1;
        return true;
    }
    return false;
}

bool ss_expect_one_of(StringScanner *ss, char const *expect)
{
    return ss_expect_one_of_with_offset(ss, expect, 0);
}

int ss_one_of(StringScanner *ss, char const *expect)
{
    if (strchr(expect, ss_peek(ss)) != NULL) {
        return ss_readchar(ss);
    }
    return 0;
}

size_t ss_read_number(StringScanner *ss)
{
    size_t ix = 0;
    while (isdigit(ss_peek_with_offset(ss, ix))) {
        ix++;
    }
    if (ix > 0) {
        StringView num = ss_read(ss, ix);
        ss_reset(ss);
        IntegerParseResult parse_result = sv_parse_u64(num);
        assert(parse_result.success);
        return parse_result.integer.u64;
    }
    return 0;
};
