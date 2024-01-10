/*
 * Copyright (c) 2023, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ctype.h>

#include <fmt.h>
#include <optional.h>

#define STATIC_ALLOCATOR
#include <allocate.h>

DA_IMPL(FMTArg)

typedef enum FormatState {
    FS_STRING = 0,
    FS_FORMAT_MAYBE,
    FS_FORMAT,
    FS_ESCAPE
} FormatState;

typedef enum FormatSpecifierType {
    FST_DEFAULT = ' ',
    FST_STRING = 'S',
    FST_INT = 'D',
    FST_CHARACTER = 'C',
    FST_GENERAL = 'G',
    FST_FIXEDPOINT = 'F',
    FST_SCIENTIFIC = 'E',
    FST_PERCENTAGE = '%',
    FST_LOCALEAWARE = 'N',
} FormatSpecifierType;

typedef enum FormatSpecifierAlignment {
    FSA_NONE = 0,
    FSA_LEFT = '<',
    FSA_RIGHT = '>',
    FSA_CENTER = '^',
    FSA_RIGHT_BUT_SIGN_LEFT = '=',
} FormatSpecifierAlignment;

typedef enum CaseCoercion {
    CC_DONTCARE = 0,
    CC_TOUPPER,
    CC_TOLOWER,
} CaseCoercion;

typedef enum DisplaySign {
    DS_ONLYFORNEGATIVE = '-',
    DS_ALWAYS = '+',
    DS_SPACEFORPOSITIVE = ' ',
} DisplaySign;

typedef enum GroupingOption {
    GO_NONE = 0,
    GO_COMMA = 1,
    GO_UNDERSCORE = 2,
    GO_SINGLE_QUOTE = 3
} GroupingOption;

typedef struct FormatSpecifier {
    FormatSpecifierType      type;
    size_t                   start;
    size_t                   length;
    int                      base;
    CaseCoercion             case_coercion;
    FormatSpecifierAlignment alignment;
    StringView               fill;
    DisplaySign              display_sign;
    GroupingOption           grouping_option;
    size_t                   width;
    size_t                   precision;
    StringView               specifier;
} FormatSpecifier;

OPTIONAL(FormatSpecifier)

typedef struct FormatString {
    StringView fmt;
    DIA(FormatSpecifier);
} FormatString;

typedef struct RenderableArgument {
    size_t          length;
    FormatSpecifier specifier;
    FMTArg          arg;
    union {
        struct {
            uint64_t    abs_val;
            int64_t     signed_val;
            uint8_t     digits[24];
            size_t      num_digits;
            char const *sign;
        } integer;
        struct {
            uint64_t    abs_int_val;
            int64_t     signed_int_val;
            uint8_t     int_digits[24];
            size_t      num_int_digits;
            uint64_t    fraction_val;
            uint8_t     fraction_digits[24];
            size_t      num_fraction_digits;
            char const *sign;
        } float_;
        StringView sv;
    };
} RenderableArgument;

typedef void (*renderable_prepare)(RenderableArgument *);
typedef void (*renderable_render)(RenderableArgument *, StringBuilder *);

typedef struct RenderType {
    renderable_prepare prepare;
    renderable_render  render;
} RenderType;

void prepare_integer(RenderableArgument *renderable)
{
    Integer         integer = renderable->arg.integer;
    FormatSpecifier specifier = renderable->specifier;

    if ((int) integer.type > 0) {
        renderable->integer.abs_val = integer_unsigned_value(integer).value;
    } else {
        renderable->integer.signed_val = integer_signed_value(integer).value;
        renderable->integer.abs_val = (renderable->integer.signed_val < 0) ? (uint64_t) -renderable->integer.signed_val : renderable->integer.signed_val;
    }
    do {
        renderable->integer.digits[renderable->integer.num_digits++] = renderable->integer.abs_val % specifier.base;
        renderable->integer.abs_val /= specifier.base;
    } while (renderable->integer.abs_val > 0);

    renderable->integer.sign = "";
    switch (specifier.display_sign) {
    case DS_ONLYFORNEGATIVE:
        if ((int) integer.type < 0 && integer_unsigned_value(integer).value < 0) {
            renderable->integer.sign = "-";
        }
        break;
    case DS_ALWAYS:
        if ((int) integer.type > 0) {
            renderable->integer.sign = "+";
        } else {
            renderable->integer.sign = (renderable->integer.signed_val < 0) ? "-" : "+";
        }
        break;
    case DS_SPACEFORPOSITIVE:
        if ((int) integer.type > 0) {
            renderable->integer.sign = " ";
        } else {
            renderable->integer.sign = (renderable->integer.signed_val < 0) ? "-" : " ";
        }
        break;
    default:
        break;
    }

    renderable->length = renderable->integer.num_digits + strlen(renderable->integer.sign);
    if (specifier.grouping_option != GO_NONE) {
        renderable->length += (renderable->integer.num_digits + 1) / 3;
    }
    if ((specifier.alignment == FSA_RIGHT_BUT_SIGN_LEFT) && (renderable->length < specifier.width)) {
        renderable->length = specifier.width;
    }
}

void render_integer(RenderableArgument *renderable, StringBuilder *sb)
{
    FormatSpecifier specifier = renderable->specifier;

    sb_append_cstr(sb, renderable->integer.sign);
    size_t length = renderable->integer.num_digits + strlen(renderable->integer.sign);
    if (specifier.grouping_option != GO_NONE) {
        length += (renderable->integer.num_digits + 1) / 3;
    }
    if (specifier.alignment == FSA_RIGHT_BUT_SIGN_LEFT) {
        for (size_t ix = strlen(renderable->integer.sign); ix < specifier.width - length; ix += specifier.fill.length) {
            sb_append_sv(sb, specifier.fill);
        }
    }

    for (int ix = (int) renderable->integer.num_digits - 1; ix >= 0; --ix) {
        sb_append_chars(sb, &"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"[renderable->integer.digits[ix]], 1);
        if ((specifier.grouping_option != GO_NONE) && ix && ((ix % 3) == 0)) {
            sb_append_chars(sb, &" ,_'"[specifier.grouping_option], 1);
        }
    }
}

void prepare_string(RenderableArgument *renderable)
{
    renderable->sv = renderable->arg.sv;
    renderable->length = renderable->sv.length;
}

void render_string(RenderableArgument *renderable, StringBuilder *sb)
{
    sb_append_sv(sb, renderable->sv);
}

void prepare_float(RenderableArgument *renderable)
{
    double          flt = renderable->arg.flt;
    FormatSpecifier specifier = renderable->specifier;

    renderable->float_.signed_int_val = (int64_t) flt;
    renderable->float_.abs_int_val = (renderable->float_.signed_int_val < 0)
        ? -renderable->float_.signed_int_val
        : renderable->float_.signed_int_val;
    double fraction = flt - (double) renderable->float_.signed_int_val;
    if (specifier.precision == 0) {
        specifier.precision = 8;
    }
    for (int ix = 0; ix < specifier.precision; ++ix) {
        fraction = fraction * 10;
    }
    renderable->float_.fraction_val = (uint64_t) fraction;

    do {
        renderable->float_.int_digits[renderable->float_.num_int_digits++] = renderable->float_.abs_int_val % 10;
        renderable->float_.abs_int_val /= 10;
    } while (renderable->float_.abs_int_val > 0);
    do {
        renderable->float_.fraction_digits[renderable->float_.num_fraction_digits++] = renderable->float_.fraction_val % 10;
        renderable->float_.fraction_val /= 10;
    } while (renderable->float_.fraction_val > 0);

    switch (specifier.display_sign) {
    case DS_ONLYFORNEGATIVE:
        if (flt < 0) {
            renderable->float_.sign = "-";
        }
        break;
    case DS_ALWAYS:
        renderable->float_.sign = (flt < 0) ? "-" : "+";
        break;
    case DS_SPACEFORPOSITIVE:
        renderable->integer.sign = (flt < 0) ? "-" : " ";
        break;
    default:
        renderable->integer.sign = "";
        break;
    }

    renderable->length = strlen(renderable->float_.sign) + renderable->float_.num_int_digits + renderable->float_.num_fraction_digits + 1;
    if (specifier.grouping_option != GO_NONE) {
        renderable->length += (renderable->float_.num_fraction_digits + 1) / 3;
    }
    if ((specifier.alignment == FSA_RIGHT_BUT_SIGN_LEFT) && (renderable->length < specifier.width)) {
        renderable->length = specifier.width;
    }
}

void render_float(RenderableArgument *renderable, StringBuilder *sb)
{
    FormatSpecifier specifier = renderable->specifier;
    if (specifier.precision == 0) {
        specifier.precision = 8;
    }

    sb_append_cstr(sb, renderable->float_.sign);
    size_t length = strlen(renderable->float_.sign) + renderable->float_.num_int_digits + renderable->float_.num_fraction_digits + 1;
    if (specifier.grouping_option != GO_NONE) {
        length += (renderable->float_.num_fraction_digits + 1) / 3;
    }
    if (specifier.alignment == FSA_RIGHT_BUT_SIGN_LEFT) {
        for (size_t ix = strlen(renderable->float_.sign); ix < specifier.width - length; ix += specifier.fill.length) {
            sb_append_sv(sb, specifier.fill);
        }
    }

    for (int ix = (int) renderable->float_.num_int_digits - 1; ix >= 0; --ix) {
        sb_append_chars(sb, &"0123456789"[renderable->float_.int_digits[ix]], 1);
        if ((specifier.grouping_option != GO_NONE) && ix && ((ix % 3) == 0)) {
            sb_append_chars(sb, &" ,_'"[specifier.grouping_option], 1);
        }
    }
    sb_append_cstr(sb, ".");
    for (size_t ix = renderable->float_.num_fraction_digits; ix < specifier.precision; ++ix) {
        sb_append_cstr(sb, "0");
    }
    for (int ix = (int) renderable->float_.num_fraction_digits - 1; ix >= 0; --ix) {
        sb_append_chars(sb, &"0123456789"[renderable->float_.fraction_digits[ix]], 1);
    }
}

static RenderType format_renderers[] = {
    [FST_INT] = { .prepare = prepare_integer, .render = render_integer },
    [FST_STRING] = { .prepare = prepare_string, .render = render_string },
    [FST_FIXEDPOINT] = { .prepare = prepare_float, .render = render_float },
    [FST_GENERAL] = { .prepare = prepare_float, .render = render_float },
};

void format_specifier_format(FormatSpecifier specifier, FMTArg arg, StringBuilder *sb)
{
    RenderableArgument renderable = { 0 };
    renderable.specifier = specifier;
    renderable.arg = arg;
    format_renderers[specifier.type].prepare(&renderable);

    if (specifier.width > renderable.length) {
        switch (specifier.alignment) {
        case FSA_NONE:
        case FSA_LEFT:
        case FSA_RIGHT_BUT_SIGN_LEFT:
            break;
        case FSA_RIGHT: {
            for (size_t ix = 0; ix < specifier.width - renderable.length; ix += specifier.fill.length) {
                sb_append_sv(sb, specifier.fill);
            }
        } break;
        case FSA_CENTER: {
            for (size_t ix = 0; ix < (specifier.width - renderable.length) / 2; ix += specifier.fill.length) {
                sb_append_sv(sb, specifier.fill);
            }
        } break;
        default:
            UNREACHABLE();
        }
    }

    format_renderers[specifier.type].render(&renderable, sb);

    if (specifier.width > renderable.length) {
        switch (specifier.alignment) {
        case FSA_NONE:
        case FSA_RIGHT:
        case FSA_RIGHT_BUT_SIGN_LEFT:
            break;
        case FSA_LEFT: {
            for (size_t ix = 0; ix < specifier.width - renderable.length; ix += specifier.fill.length) {
                sb_append_sv(sb, specifier.fill);
            }
        } break;
        case FSA_CENTER: {
            for (size_t ix = (specifier.width + renderable.length) / 2; ix < specifier.width; ix += specifier.fill.length) {
                sb_append_sv(sb, specifier.fill);
            }
        } break;
        default:
            UNREACHABLE();
        }
    }

    switch (specifier.case_coercion) {
    case CC_TOLOWER: {
        char *ptr = (char *) sb->view.ptr;
        for (size_t ix = 0; ix < sb->view.length; ++ix) {
            ptr[ix] = (char) tolower(ptr[ix]);
        }
    } break;
    case CC_TOUPPER: {
        char *ptr = (char *) sb->view.ptr;
        for (size_t ix = 0; ix < sb->view.length; ++ix) {
            ptr[ix] = (char) toupper(ptr[ix]);
        }
    } break;
    default:
        break;
    }
}

FormatSpecifier format_specifier_init(StringView msg, size_t start, size_t len)
{
    FormatSpecifier ret = { 0 };
    ret.type = FST_DEFAULT;
    ret.base = 10;
    ret.alignment = FSA_LEFT;
    ret.fill = sv_from(" ");
    ret.display_sign = DS_ONLYFORNEGATIVE;
    ret.start = start;
    ret.length = len;
    ret.specifier = (StringView) { msg.ptr + start, len };

    StringScanner scanner = ss_create(ret.specifier);
    if (!ss_expect(&scanner, '{')) {
        fatal("Expected '{'\n");
    }

    ss_reset(&scanner);
    if (ss_is_one_of_with_offset(&scanner, "<>=^", 1) != 0) {
        ret.fill = ss_read(&scanner, 1);
        ss_reset(&scanner);
    }
    int alignment = ss_one_of(&scanner, "<>=^");
    if (alignment != 0) {
        ret.alignment = (FormatSpecifierAlignment) alignment;
        ss_reset(&scanner);
    }
    int display_sign = ss_one_of(&scanner, " +-");
    if (display_sign != 0) {
        ret.display_sign = (DisplaySign) display_sign;
        ss_reset(&scanner);
    }
    if (ss_peek(&scanner) == '0') {
        ret.alignment = FSA_RIGHT_BUT_SIGN_LEFT;
        ret.fill = sv_from("0");
        ss_skip_one(&scanner);
        ss_reset(&scanner);
    }

    ret.width = ss_read_number(&scanner);
    GroupingOption grouping_option = (GroupingOption) ss_one_of(&scanner, ",_");
    if (grouping_option != 0) {
        ret.grouping_option = grouping_option;
        ss_reset(&scanner);
    }
    if (ss_peek(&scanner) == '.') {
        ss_skip_one(&scanner);
        ss_reset(&scanner);
        ret.precision = ss_read_number(&scanner);
        if (ret.precision == 0) {
            fatal("Syntax error in format specifier: expected number following '.'");
        }
    }

    switch (ss_peek(&scanner)) {
    case 's':
        ret.type = FST_STRING;
        break;
    case 'b':
        ret.type = FST_INT;
        ret.base = 2;
        break;
    case 'c':
        ret.type = FST_CHARACTER;
        break;
    case 'd':
        ret.type = FST_INT;
        ret.base = 10;
        break;
    case 'n':
        ret.type = FST_LOCALEAWARE;
        break;
    case 'o':
        ret.type = FST_INT;
        ret.base = 8;
        break;
    case 'X':
        ret.type = FST_INT;
        ret.base = 16;
        ret.case_coercion = CC_TOUPPER;
        break;
    case 'x':
        ret.type = FST_INT;
        ret.base = 16;
        ret.case_coercion = CC_TOLOWER;
        break;

    case 'e':
        ret.type = FST_SCIENTIFIC;
        ret.case_coercion = CC_TOLOWER;
        break;
    case 'E':
        ret.type = FST_SCIENTIFIC;
        ret.case_coercion = CC_TOUPPER;
        break;
    case 'f':
        ret.type = FST_FIXEDPOINT;
        ret.case_coercion = CC_TOLOWER;
        break;
    case 'F':
        ret.type = FST_FIXEDPOINT;
        ret.case_coercion = CC_TOUPPER;
        break;
    case 'g':
        ret.type = FST_GENERAL;
        ret.case_coercion = CC_TOLOWER;
        break;
    case 'G':
        ret.type = FST_GENERAL;
        ret.case_coercion = CC_TOUPPER;
        break;
    case '%':
        ret.type = FST_PERCENTAGE;
        break;

    default:
        ret.type = FST_DEFAULT;
        break;
    }
    return ret;
}

OptionalFormatSpecifier first_specifier(StringView msg)
{
    FormatState state = FS_STRING;
    int         start = -1;

    for (int ix = 0; ix < msg.length; ix++) {
        char ch = msg.ptr[ix];
        switch (state) {
        case FS_STRING:
            if (ch == '{') {
                state = FS_FORMAT_MAYBE;
                start = ix;
            }
            break;
        case FS_FORMAT_MAYBE:
            if (ch == '{') {
                state = FS_STRING;
                start = -1;
                break;
            }
            state = FS_FORMAT;
            /* Fall through */
        case FS_FORMAT:
            switch (ch) {
            case '\\':
                state = FS_ESCAPE;
                break;
            case '}':
                return OptionalFormatSpecifier_create(format_specifier_init(msg, start, ix - start + 1));
            default:
                break;
            }
            break;
        case FS_ESCAPE:
            break;
        }
    }
    return OptionalFormatSpecifier_empty();
}

FormatString fmt_parse(StringView fmt)
{
    FormatString  fs = { 0 };
    FormatString *fs_ptr = &fs;
    fs.fmt = fmt;
    while (true) {
        OptionalFormatSpecifier specifier_maybe = first_specifier(fmt);
        if (!specifier_maybe.has_value) {
            return fs;
        }
        DIA_APPEND(FormatSpecifier, fs_ptr, specifier_maybe.value);
        fmt = sv_lchop(fmt, specifier_maybe.value.start + specifier_maybe.value.length);
    }
}

StringView format_string_format(FormatString fs, FMTArgs args)
{
    StringBuilder sb = sb_create();
    size_t        offset = 0;
    StringView    fmt = fs.fmt;
    for (size_t ix = 0; ix < fs.size; ++ix) {
        sb_append_sv(&sb, sv_substring(fmt, 0, fs.elements[ix].start));
        fmt = sv_lchop(fmt, fs.elements[ix].start + fs.elements[ix].length);
        format_specifier_format(fs.elements[ix], args.elements[ix], &sb);
    }
    sb_append_sv(&sb, sv_lchop(fmt, offset));
    return sb.view;
}

StringView fmt_format(StringView fmt, FMTArgs args)
{
    FormatString fs = fmt_parse(fmt);
    if (fs.size != args.size) {
        fatal("Invalid number of arguments for format string '%.*s'. Expected %zu, got %zu",
            SV_ARG(fmt), fs.size, args.size);
    }
    return format_string_format(fs, args);
}

StringView vformat(StringView fmt, va_list args) // NOLINT(readability-non-const-parameter)
{
    FormatString fs = fmt_parse(fmt);
    FMTArgs      fmt_args;
    for (size_t ix = 0; ix < fs.size; ++ix) {
        FormatSpecifier s = fs.elements[ix];
        switch (s.type) {
        case FST_INT: {
            int i = va_arg(args, int);
            da_append_FMTArg(&fmt_args, (FMTArg) { .integer = { .type = I32, .i32 = i } });
        } break;
        case FST_STRING: {
            StringView sv = sv_from(va_arg(args, char *));
            da_append_FMTArg(&fmt_args, (FMTArg) { .sv = sv });
        } break;
        default:
            NYI("FormatSpecifierType %d", s.type);
        }
    }
    return format_string_format(fs, fmt_args);
}

StringView format(StringView fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    StringView ret = vformat(fmt, args);
    va_end(args);
    return ret;
}

// #define FMT_TEST
#ifdef FMT_TEST

int main()
{
    StringView sv = format(sv_from("{s}"), "Hello, World");
    printf("--%.*s--\n", SV_ARG(sv));
    sv = format(sv_from("{d}"), 42);
    printf("--%.*s--\n", SV_ARG(sv));
    sv = format(sv_from("Hello {d} World"), 42);
    printf("--%.*s--\n", SV_ARG(sv));

    return 0;
}

#endif
