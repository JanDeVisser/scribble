/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#include <allocate.h>
#include <stddef.h>
#include <sv.h>

DECLARE_SHARED_ALLOCATOR(sv)

char  *allocate_for_length(size_t length, size_t *capacity);
size_t buffer_capacity(char const *buffer);

StringView sv_from(char const *s)
{
    size_t     len = (s) ? strlen(s) : 0;
    StringView ret = { 0 };
    if (len) {
        ret.ptr = s;
        ret.length = len;
    }
    return ret;
}

StringView sv_copy(StringView sv)
{
    if (sv_empty(sv)) {
        return sv;
    }
    return sv_copy_chars(sv.ptr, sv.length);
}

StringView sv_copy_chars(char const *ptr, size_t len)
{
    if (!ptr || !len) {
        return sv_null();
    }
    StringView ret = { 0 };
    ret.ptr = allocate_for_length(len + 1, NULL);
    ret.length = len;
    memcpy((char *) ret.ptr, ptr, len);
    ((char *) ret.ptr)[ret.length] = 0;
    return ret;
}

StringView sv_copy_cstr(char const *s)
{
    size_t len = strlen(s);
    if (!s || !len) {
        return sv_null();
    }
    return sv_copy_chars(s, len);
}

StringView sv_render_integer(Integer integer)
{
    StringView ret;
    switch (integer.type) {
    case U8:
        ret = sv_printf("%u", integer.u8);
        break;
    case U16:
        ret = sv_printf("%u", integer.u16);
        break;
    case U32:
        ret = sv_printf("%u", integer.u32);
        break;
    case U64:
        ret = sv_printf("%lu", integer.u64);
        break;
    case I8:
        ret = sv_printf("%d", integer.i8);
        break;
    case I16:
        ret = sv_printf("%d", integer.i16);
        break;
    case I32:
        ret = sv_printf("%d", integer.i32);
        break;
    case I64:
        ret = sv_printf("%ld", integer.i64);
        break;
    default:
        UNREACHABLE();
    }
    return ret;
}

StringView sv_render_hex_integer(Integer integer)
{
    StringView ret;
    switch (integer.type) {
    case U8:
        ret = sv_printf("%1x", integer.u8);
        break;
    case U16:
        ret = sv_printf("%02x", integer.u16);
        break;
    case U32:
        ret = sv_printf("%04x", integer.u32);
        break;
    case U64:
        ret = sv_printf("%08x", integer.u64);
        break;
    default:
        UNREACHABLE();
    }
    return ret;
}

StringView sv_replicate(StringView s, int repeats)
{
    StringBuilder sb = sb_create();
    for (int ix = 0; ix < repeats; ++ix) {
        sb_append_sv(&sb, s);
    }
    return sb.view;
}

StringView sv_decode_quoted_str(StringView str)
{
    assert(sv_length(str) >= 2 && str.ptr[0] == '\"' && str.ptr[sv_length(str) - 1] == '\"');
    int backslashes = 0;
    for (size_t ix = 1; ix < sv_length(str) - 1; ++ix) {
        if (str.ptr[ix] == '\\') {
            ++ix;
            ++backslashes;
        }
    }
    if (!backslashes) {
        StringView ret = { str.ptr + 1, str.length - 2 };
        return ret;
    }
    bool   prev_backslash = false;
    size_t len = sv_length(str) - 2 - backslashes;
    char  *buffer = allocate(len);
    char  *ptr = buffer;
    for (size_t ix = 1; ix < sv_length(str) - 1; ++ix) {
        if (prev_backslash || str.ptr[ix] != '\\') {
            char ch;
            switch (str.ptr[ix]) {
            case 'n':
                ch = '\n';
                break;
            case 't':
                ch = '\t';
                break;
            default:
                ch = str.ptr[ix];
            }
            *ptr++ = ch;
            prev_backslash = false;
        } else if (str.ptr[ix] == '\\') {
            prev_backslash = true;
        }
    }
    StringView ret = { buffer, len };
    return ret;
}

StringView sv_replace(StringView str, StringView from, StringView to)
{
    StringView ret = { 0 };
    if (sv_empty(str) || sv_empty(from)) {
        return str;
    }
    StringBuilder sb = sb_create();
    size_t        ix = 0;
    while (ix < sv_length(str)) {
        if (ix + sv_length(from) <= sv_length(str) && !memcmp(str.ptr + ix, from.ptr, from.length)) {
            sb_append_sv(&sb, to);
            ix += from.length;
        } else {
            sb_append_chars(&sb, str.ptr + ix, 1);
            ++ix;
        }
    }
    ret = sb_view(&sb);
    return ret;
}

void sv_free(StringView sv)
{
    free_buffer((char *) sv.ptr);
}

StringView sv_null()
{
    StringView ret = { 0 };
    return ret;
}

StringView sv_printf(char const *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    StringView ret = sv_vprintf(fmt, args);
    va_end(args);
    return ret;
}

StringView sv_vprintf(char const *fmt, va_list args)
{
    StringBuilder builder = sb_vcreatef(fmt, args);
    return builder.view;
}

bool sv_empty(StringView sv)
{
    return sv.length == 0;
}

bool sv_not_empty(StringView sv)
{
    return sv.length > 0;
}

size_t sv_length(StringView sv)
{
    return sv.length;
}

bool sv_is_cstr(StringView sv)
{
    bool ret = sv.ptr && (sv.ptr[sv.length] == 0);
    if (!ret && (buffer_capacity(sv.ptr) > sv.length)) {
        char *ptr = (char *) sv.ptr;
        ptr[sv.length] = '\0';
        ret = true;
    }
    return ret;
}

char const *sv_cstr(StringView sv)
{
    if (sv_is_cstr(sv)) {
        return sv.ptr;
    } else {
        char *cstr = allocate_for_length(sv.length + 1, NULL);
        memcpy(cstr, sv.ptr, sv.length);
        cstr[sv.length] = '\0';
        return cstr;
    }
}

int sv_cmp(StringView s1, StringView s2)
{
    if (s1.length != s2.length) {
        return ((int) s1.length) - ((int) s2.length);
    }
    if (s1.length == 0 && s2.length == 0) {
        return 0;
    }
    return memcmp(s1.ptr, s2.ptr, s1.length);
}

bool sv_eq(StringView s1, StringView s2)
{
    if (s1.length != s2.length) {
        return false;
    }
    if (s1.length == 0 && s2.length == 0) {
        return true;
    }
    return memcmp(s1.ptr, s2.ptr, s1.length) == 0;
}

bool sv_eq_cstr(StringView s1, char const *s2)
{
    size_t len = (s2) ? strlen(s2) : 0;
    if (s1.length != len) {
        return false;
    }
    if (s1.length == 0 && len == 0) {
        return true;
    }
    return memcmp(s1.ptr, s2, s1.length) == 0;
}

bool sv_eq_chars(StringView s1, char const *s2, size_t n)
{
    if (s1.length != n) {
        return false;
    }
    if (s1.length == 0 && (!s2 || !*s2)) {
        return true;
    }
    return memcmp(s1.ptr, s2, n) == 0;
}

bool sv_eq_ignore_case(StringView s1, StringView s2)
{
    return sv_eq_ignore_case_chars(s1, s2.ptr, s2.length);
}

bool sv_eq_ignore_case_cstr(StringView s1, char const *s2)
{
    return sv_eq_ignore_case_chars(s1, s2, strlen(s2));
}

bool sv_eq_ignore_case_chars(StringView s1, char const *s2, size_t n)
{
    if (s1.length != n) {
        return false;
    }
    if (s1.length == 0 && (!s2 || !*s2)) {
        return true;
    }
    for (size_t ix = 0; ix < n; ++ix) {
        if (toupper(s1.ptr[ix]) != toupper(s2[ix])) {
            return false;
        }
    }
    return true;
}

bool sv_startswith(StringView s1, StringView s2)
{
    if (!s2.length || s1.length < s2.length) {
        return false;
    }
    return memcmp(s1.ptr, s2.ptr, s2.length) == 0;
}

bool sv_endswith(StringView s1, StringView s2)
{
    if (!s2.length || s1.length < s2.length) {
        return false;
    }
    return memcmp(s1.ptr + (s1.length - s2.length), s2.ptr, s2.length) == 0;
}

StringView sv_lchop(StringView sv, size_t num)
{
    if (num >= sv.length) {
        return sv_null();
    }
    StringView ret = { sv.ptr + num, sv.length - num };
    return ret;
}

StringView sv_rchop(StringView sv, size_t num)
{
    if (num >= sv.length) {
        return sv_null();
    }
    StringView ret = { sv.ptr, sv.length - num };
    return ret;
}

StringView sv_chop_to_delim(StringView *src, StringView delim)
{
    StringView ret = *src;
    if (sv_empty(ret) || sv_empty(delim) || ret.length < delim.length) {
        return sv_null();
    }
    int ix = sv_find(ret, delim);
    if (ix == -1) {
        *src = sv_null();
        return ret;
    }
    if (ret.length - ix - delim.length == 0) {
        *src = sv_null();
    } else {
        *src = (StringView) { ret.ptr + ix + delim.length, ret.length - ix - delim.length };
    }
    ret.length = ix;
    return ret;
}

int sv_first(StringView sv, char ch)
{
    for (int ix = 0; ix < sv.length; ++ix) {
        if (*(sv.ptr + ix) == ch)
            return ix;
    }
    return -1;
}

int sv_last(StringView sv, char ch)
{
    for (int ix = 0; ix < sv.length; ++ix) {
        if (*(sv.ptr + (sv.length - ix - 1)) == ch)
            return (int) sv.length - ix - 1;
    }
    return -1;
}

int sv_find(StringView sv, StringView sub)
{
    for (int ix = 0; ix <= sv.length - sub.length; ++ix) {
        if (!memcmp(sv.ptr + ix, sub.ptr, sub.length))
            return ix;
    }
    return -1;
}

StringView sv_substring(StringView sv, size_t at, size_t len)
{
    if (at >= sv.length) {
        return sv_null();
    }
    if (at + len > sv.length) {
        len = sv.length - at;
    }
    return (StringView) { sv.ptr + at, len };
}

StringList sv_split(StringView sv, StringView sep)
{
    assert(sep.length > 0);
    StringList ret = sl_create();
    if (sv.length == 0) {
        return ret;
    }
    char const *ptr = sv.ptr;
    char const *component_start = sv.ptr;
    while (true) {
        if (ptr - sv.ptr > (ptrdiff_t) sv.length - (ptrdiff_t) sep.length) {
            sl_push(&ret, (StringView) { component_start, sv.ptr + sv.length - component_start });
            return ret;
        }
        if (memcmp(ptr, sep.ptr, sep.length) == 0) {
            sl_push(&ret, (StringView) { component_start, ptr - component_start });
            do {
                ptr += sep.length;
                component_start = ptr;
            } while (memcmp(ptr, sep.ptr, sep.length) == 0);
            continue;
        }
        ++ptr;
    }
    UNREACHABLE();
}

StringList sv_split_by_whitespace(StringView sv)
{
    StringList  ret = sl_create();
    char const *current = NULL;
    for (size_t ix = 0; ix < sv.length; ++ix) {
        if (isspace(sv.ptr[ix]) && current) {
            sl_push(&ret, (StringView) { current, (sv.ptr + ix) - current });
            current = NULL;
        }
        if (!isspace(sv.ptr[ix]) && !current) {
            current = sv.ptr + ix;
        }
    }
    if (current) {
        sl_push(&ret, (StringView) { current, (sv.ptr + sv.length) - current });
    }
    return ret;
}

StringView sv_strip(StringView sv)
{
    StringView ret = { 0 };
    if (!sv.length) {
        return sv_null();
    }
    for (ret.ptr = sv.ptr; (ret.ptr - sv.ptr < sv.length) && isspace(*ret.ptr); ++ret.ptr)
        ;
    if (ret.ptr - sv.ptr >= sv.length) {
        return sv_null();
    }
    for (ret.length = sv.length - (ret.ptr - sv.ptr);
         ret.length > 0 && isspace(ret.ptr[ret.length - 1]);
         --ret.length)
        ;
    if (!ret.length) {
        ret.ptr = NULL;
    }
    return ret;
}

bool char_is_digit_in_base(int ch, size_t base)
{
    assert(base > 1 && base <= 36);
    if (base <= 10) {
        return isdigit(ch) && (ch < '0' + base);
    }
    ch = toupper(ch);
    return isdigit(ch) || ((ch >= 'A') && (ch < 'A' + base - 10));
}

int digit_value_in_base(int ch, size_t base)
{
    assert(base > 1 && base <= 36);
    assert(char_is_digit_in_base(ch, base));
    if (ch <= '9') {
        return ch - '0';
    }
    ch = toupper(ch);
    return ch - 'A' + 10;
}

// Wholesale theft from
//   https://github.com/pts/pts-parse-int/blob/master/parse_dec.cc
// and
//   https://ptspts.blogspot.com/2014/04/how-to-parse-integer-in-c-and-c-with.html

#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)                                                      \
    IntegerParseResult sv_parse_u##sz(StringView sv)                          \
    {                                                                         \
        IntegerParseResult ret = { 0 };                                       \
        ret.integer.type = U##sz;                                             \
        if (sv.length == 0) {                                                 \
            return ret;                                                       \
        }                                                                     \
        size_t  ix = 0;                                                       \
        uint8_t base = 10;                                                    \
        while (ix < sv.length && isspace(sv.ptr[ix])) {                       \
            ++ix;                                                             \
        }                                                                     \
        if (ix >= sv.length) {                                                \
            return ret;                                                       \
        }                                                                     \
        if (ix < sv.length - 2 && sv.ptr[ix] == '0') {                        \
            if (sv.ptr[ix + 1] == 'x' || sv.ptr[ix + 1] == 'X') {             \
                base = 16;                                                    \
                ix += 2;                                                      \
            } else if (sv.ptr[ix + 1] == 'b' || sv.ptr[ix + 1] == 'B') {      \
                base = 2;                                                     \
                ix += 2;                                                      \
            }                                                                 \
        }                                                                     \
        if (!char_is_digit_in_base(sv.ptr[ix], base)) {                       \
            return ret;                                                       \
        }                                                                     \
                                                                              \
        uint##sz##_t nmax = ((uint##sz##_t) ~(uint##sz##_t) 0) / 10;          \
        while (ix < sv.length && char_is_digit_in_base(sv.ptr[ix], base)) {   \
            int           c = digit_value_in_base(sv.ptr[ix], base);          \
            const uint8_t nneg = -ret.integer.u##sz;                          \
            if (nneg > nmax || (nneg == nmax && c > 5)) {                     \
                return ret;                                                   \
            }                                                                 \
            ret.integer.u##sz = base * ret.integer.u##sz + c;                 \
            ix++;                                                             \
        }                                                                     \
        ret.success = true;                                                   \
        ret.tail.length = sv.length - ix;                                     \
        ret.tail.ptr = (ret.tail.length) ? sv.ptr + ix : NULL;                \
        return ret;                                                           \
    }                                                                         \
                                                                              \
    IntegerParseResult sv_parse_i##sz(StringView sv)                          \
    {                                                                         \
        IntegerParseResult ret = { 0 };                                       \
        ret.integer.type = I##sz;                                             \
        if (sv.length == 0) {                                                 \
            return ret;                                                       \
        }                                                                     \
        size_t ix = 0;                                                        \
        int8_t base = 10;                                                     \
        while (ix < sv.length && isspace(sv.ptr[ix])) {                       \
            ++ix;                                                             \
        }                                                                     \
        if (ix >= sv.length) {                                                \
            return ret;                                                       \
        }                                                                     \
        bool negative = sv.ptr[ix] == '-';                                    \
        while (ix < sv.length && isspace(sv.ptr[ix])) {                       \
            ++ix;                                                             \
        }                                                                     \
        if (ix >= sv.length) {                                                \
            return ret;                                                       \
        }                                                                     \
        if (ix < sv.length - 2 && sv.ptr[ix] == '0') {                        \
            if (sv.ptr[ix + 1] == 'x' || sv.ptr[ix + 1] == 'X') {             \
                base = 16;                                                    \
                ix += 2;                                                      \
            } else if (sv.ptr[ix + 1] == 'b' || sv.ptr[ix + 1] == 'B') {      \
                base = 2;                                                     \
                ix += 2;                                                      \
            }                                                                 \
        }                                                                     \
        if (!char_is_digit_in_base(sv.ptr[ix], base)) {                       \
            return ret;                                                       \
        }                                                                     \
                                                                              \
        int##sz##_t nmax = (int##sz##_t) ~((int##sz##_t) 1 << (sz - 1)) / 10; \
        char        cmax = (negative) ? 8 : 7;                                \
        int##sz##_t n = 0;                                                    \
        while (ix < sv.length && char_is_digit_in_base(sv.ptr[ix], base)) {   \
            int          c = digit_value_in_base(sv.ptr[ix], base);           \
            const int8_t nneg = -n;                                           \
            if (nneg > nmax || (nneg == nmax && c > cmax)) {                  \
                return ret;                                                   \
            }                                                                 \
            n = base * n - c;                                                 \
            ix++;                                                             \
        }                                                                     \
        ret.success = true;                                                   \
        ret.tail.length = sv.length - ix;                                     \
        ret.tail.ptr = (ret.tail.length) ? sv.ptr + ix : NULL;                \
        ret.integer.i##sz = (negative) ? n : -n;                              \
        return ret;                                                           \
    }
INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE

IntegerParseResult sv_parse_integer(StringView sv, IntegerType type)
{
    switch (type) {
#undef INTEGER_SIZE
#define INTEGER_SIZE(sz)           \
    case U##sz:                    \
        return sv_parse_u##sz(sv); \
    case I##sz:                    \
        return sv_parse_i##sz(sv);
        INTEGER_SIZES(INTEGER_SIZE)
#undef INTEGER_SIZE
    default:
        UNREACHABLE();
    }
}

#ifdef SV_TEST

void test_split_join(StringView sv)
{
    StringList split = sv_split_by_whitespace(sv);
    printf("split size: %zu\n", split.size);
    for (size_t ix = 0; ix < split.size; ++ix) {
        printf("split[%zu] = %.*s\n", ix, SV_ARG(split.strings[ix]));
    }
    StringView joined = sl_join(&split, sv_from("*"));
    printf("--%.*s--\n", SV_ARG(joined));
}

int main()
{
    printf("--%.*s--\n", SV_ARG(sv_strip(sv_from("  abcd \t"))));
    printf("--%.*s--\n", SV_ARG(sv_strip(sv_from("abcd \t"))));
    printf("--%.*s--\n", SV_ARG(sv_strip(sv_from("  abcd"))));

    test_split_join(sv_from("  ab cd ef  "));
    test_split_join(sv_from("ab cd ef  "));
    test_split_join(sv_from("  ab \t cd \n\n ef"));

    IntegerParseResult res = sv_parse_u64(sv_from("69"));
    if (res.success) {
        printf("Sixtynine u64 = %llu\n", res.integer.u64);
    }
    res = sv_parse_u32(sv_from("69"));
    if (res.success) {
        printf("Sixtynine u32 = %u\n", res.integer.u32);
    }
    res = sv_parse_i64(sv_from("69"));
    if (res.success) {
        printf("Sixtynine i64 = %lld\n", res.integer.i64);
    }
    res = sv_parse_i32(sv_from("69"));
    if (res.success) {
        printf("Sixtynine i32 = %d\n", res.integer.i32);
    }
    return 0;
}

#endif
