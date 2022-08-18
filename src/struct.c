/**
 * struct.c
 * Structure packing like python 'struct' module.
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2013 Mozzhuhin Andrey
 * Copyright (c) 2021, Joel
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <errno.h>

#include "struct.h"

struct struct_context{
    const char *format;
    int byte_order;
    int align;
    char *buf;
    uint32_t buf_limit;
    uint32_t buf_offset;
};

typedef ssize_t (*pack_func)(struct struct_context *ctx, va_list *va, int repeat);
typedef ssize_t (*unpack_func)(struct struct_context *ctx, va_list *va, int repeat);
struct struct_format_type{
    pack_func pack;
    unpack_func unpack;
    size_t size;
};

static inline void parse_flag(struct struct_context *ctx){
    switch(ctx->format[0]){
    case '<':
        ctx->byte_order = LITTLE_ENDIAN;
        ctx->format += 1;
        break;
    case '>':
    case '!':
        ctx->byte_order = BIG_ENDIAN;
        ctx->format += 1;
        break;
    case '=':
        ctx->byte_order = BYTE_ORDER;
        ctx->format += 1;
        break;
    case '@':
        ctx->format += 1;
    default: //default flag
        ctx->byte_order = BYTE_ORDER;
        ctx->align = 1;
        break;
    }
}

static inline int format_has_next(struct struct_context *ctx){
    return (int)(*(ctx->format));
}

static inline int parse_repeat(struct struct_context *ctx){
    int repeat = 1;
    char *c = ctx->format;
    if(isdigit(*c)){
        repeat = 0;
        do{
            repeat = repeat * 10 + *c - '0';
            c++;
        }while(isdigit(*c));
    }
    ctx->format = c;
    return repeat;
}

static inline char parse_type(struct struct_context *ctx){
    char type = ctx->format[0];
    if('l' == type || 'L' == type){ //long is very special
        if(ctx->align){ //'@' flag has been set 
#ifdef __x86_64__
            type = ('l' == type ? 'q' : 'Q'); //as int64_t or uint64_t
#else
            type = ('l' == type ? 'i' : 'I'); //as int32_t or uint32_t
#endif
        }else{
            type = ('l' == type ? 'i' : 'I'); //as int32_t or uint32_t
        }
    }
    ctx->format += 1;
    return type;
}

#define CALC_ALIGN(align, offset) \
    ({ \
        (((offset) + (align) - 1) / (align)) * (align); \
    })

///////////////////////////////////////////////////////////////////////////////
// swab number
///////////////////////////////////////////////////////////////////////////////
#define DEFINE_NUM_SWAB(T) \
    static T swab_##T(T value){ \
        union{ \
            T v; char c[8]; \
        } num1 = {.v = (value)}, num2; \
        size_t len = sizeof(T); \
        int i = 0, s = 0, e = len-1; \
        for (; i < len; i++, s++, e--){ \
            num2.c[s] = num1.c[e]; \
        } \
        return num2.v; \
    }
DEFINE_NUM_SWAB(uint16_t)
DEFINE_NUM_SWAB(uint32_t)
DEFINE_NUM_SWAB(uint64_t)
DEFINE_NUM_SWAB(float)
DEFINE_NUM_SWAB(double)

///////////////////////////////////////////////////////////////////////////////
// serialize
///////////////////////////////////////////////////////////////////////////////
enum struct_number_types{
    UINT8, UINT16, UINT32, UINT64,
    INT8, INT16, INT32, INT64,
    FLOAT, DOUBLE,
};

static ssize_t pack_number(char *buf, int byte_order, int type, va_list *va) {
    switch(type){
    case INT8:
    case UINT8:{
        uint8_t i = (uint8_t)va_arg(*va, int); //type must be int!
        memcpy(buf, &i, sizeof(uint8_t));
        return sizeof(uint8_t);
    }
    case INT16:
    case UINT16:{
        uint16_t i = (uint16_t)va_arg(*va, int); //type must be int!
        if(byte_order != BYTE_ORDER)
            i = swab_uint16_t(i);
        memcpy(buf, &i, sizeof(uint16_t));
        return sizeof(uint16_t);
    }
    case INT32:
    case UINT32:{
        uint32_t i = va_arg(*va, uint32_t);
        if(byte_order != BYTE_ORDER)
            i = swab_uint32_t(i);
        memcpy(buf, &i, sizeof(uint32_t));
        return sizeof(uint32_t);
    }
    case INT64:
    case UINT64:{
        uint64_t i = va_arg(*va, uint64_t);
        if(byte_order != BYTE_ORDER)
            i = swab_uint64_t(i);
        memcpy(buf, &i, sizeof(uint64_t));
        return sizeof(uint64_t);
    }
    case FLOAT:{
        float f = (float)va_arg(*va, double); //type must be double!
        if(byte_order != BYTE_ORDER)
            f = swab_float(f);
        memcpy(buf, &f, sizeof(float));
        return sizeof(float);
    }
    case DOUBLE:{
        double d = va_arg(*va, double);
        if(byte_order != BYTE_ORDER)
            d = swab_double(d);
        memcpy(buf, &d, sizeof(double));
        return sizeof(double);
    }
    default:
        return -1;
    }
}
#define DEFINE_NUM_PACK(T,t,a) \
    static ssize_t pack_##T(struct struct_context *ctx, va_list *va, int repeat){ \
        ssize_t offset = ctx->buf_offset, i = 0; \
        if(ctx->align && 1 < (a)){ \
            offset = CALC_ALIGN((a), offset); \
        } \
        for(; i < repeat; i++){ \
            if((offset + (a)) > ctx->buf_limit){ \
                return -1; \
            } \
            ssize_t size = pack_number(ctx->buf + offset, ctx->byte_order, (t), va); \
            if(0 >= size){ \
                return -1; \
            } \
            offset += size; \
        } \
        ssize_t packed_size = offset - ctx->buf_offset; \
        ctx->buf_offset = offset; \
        return packed_size; \
    }
DEFINE_NUM_PACK(uint8, UINT8, 1)
DEFINE_NUM_PACK(uint16, UINT16, 2)
DEFINE_NUM_PACK(uint32, UINT32, 4)
DEFINE_NUM_PACK(uint64, UINT64, 8)
DEFINE_NUM_PACK(int8, INT8, 1)
DEFINE_NUM_PACK(int16, INT16, 2)
DEFINE_NUM_PACK(int32, INT32, 4)
DEFINE_NUM_PACK(int64, INT64, 8)
DEFINE_NUM_PACK(float, FLOAT, 4)
DEFINE_NUM_PACK(double, DOUBLE, 8)

static ssize_t unpack_number(char *buf, int byte_order, int type, va_list *va){
    switch(type){
    case INT8:
    case UINT8:{
        uint8_t i = *(uint8_t*)(buf);
        *va_arg(*va, uint8_t*) = i;
        return sizeof(uint8_t);
    }
    case INT16:
    case UINT16:{
        uint16_t i = *(uint16_t*)(buf);
        if(byte_order != BYTE_ORDER)
            i = swab_uint16_t(i);
        *va_arg(*va, uint16_t*) = i;
        return sizeof(uint16_t);
    }
    case INT32:
    case UINT32:{
        uint32_t i = *(uint32_t*)(buf);
        if(byte_order != BYTE_ORDER)
            i = swab_uint32_t(i);
        *va_arg(*va, uint32_t*) = i;
        return sizeof(uint32_t);
    }
    case INT64:
    case UINT64:{
        uint64_t i = *(uint64_t*)(buf);
        if(byte_order != BYTE_ORDER)
            i = swab_uint64_t(i);
        *va_arg(*va, uint64_t*) = i;
        return sizeof(uint64_t);
    }
    case FLOAT:{
        float f = *(float*)(buf);
        if(byte_order != BYTE_ORDER)
            f = swab_float(f);
        *va_arg(*va, float*) = f;
        return sizeof(float);
    }
    case DOUBLE:{
        double d = *(double*)(buf);
        if(byte_order != BYTE_ORDER)
            d = swab_double(d);
        *va_arg(*va, double*) = d;
        return sizeof(double);
    }
    default:
        return -1;
    }
}
#define DEFINE_NUM_UNPACK(T,t,a) \
    static ssize_t unpack_##T(struct struct_context *ctx, va_list *va, int repeat){ \
        ssize_t offset = ctx->buf_offset, i = 0; \
        if(ctx->align && 1 < (a)){ \
            offset = CALC_ALIGN((a), offset); \
        } \
        for(; i < repeat; i++){ \
            if((offset + (a)) > ctx->buf_limit){ \
                return -1; \
            } \
            ssize_t szie = unpack_number(ctx->buf + offset, ctx->byte_order, (t), va); \
            if(0 >= szie){ \
                return -1; \
            } \
            offset += szie; \
        } \
        ssize_t unpacked_szie = offset - ctx->buf_offset; \
        ctx->buf_offset = offset; \
        return unpacked_szie; \
    }
DEFINE_NUM_UNPACK(uint8, UINT8, 1)
DEFINE_NUM_UNPACK(uint16, UINT16, 2)
DEFINE_NUM_UNPACK(uint32, UINT32, 4)
DEFINE_NUM_UNPACK(uint64, UINT64, 8)
DEFINE_NUM_UNPACK(int8, INT8, 1)
DEFINE_NUM_UNPACK(int16, INT16, 2)
DEFINE_NUM_UNPACK(int32, INT32, 4)
DEFINE_NUM_UNPACK(int64, INT64, 8)
DEFINE_NUM_UNPACK(float, FLOAT, 4)
DEFINE_NUM_UNPACK(double, DOUBLE, 8)

static ssize_t pack_pad(struct struct_context *ctx, va_list *va, int repeat){
    size_t packed_size = sizeof(char) * repeat;
    if((ctx->buf_offset + packed_size) > ctx->buf_limit){
        return -1;
    }
    memset(ctx->buf + ctx->buf_offset, 0, packed_size);
    ctx->buf_offset += packed_size;
    return sizeof(char) * repeat;
}

static ssize_t unpack_pad(struct struct_context *ctx, va_list *va, int repeat){
    size_t unpacked_size = sizeof(char) * repeat;
    if((ctx->buf_offset + unpacked_size) > ctx->buf_limit){
        return -1;
    }
    ctx->buf_offset += unpacked_size;
    return (ssize_t)unpacked_size;
}

static ssize_t pack_str(struct struct_context *ctx, va_list *va, int len) {
    size_t packed_size = sizeof(char) * len;
    if((ctx->buf_offset + packed_size) > ctx->buf_limit){
        return -1;
    }
    memcpy(ctx->buf + ctx->buf_offset, va_arg(*va, char *), packed_size);
    ctx->buf_offset += packed_size;
    return (ssize_t)packed_size;
}

static ssize_t unpack_str(struct struct_context *ctx, va_list *va, int len) {
    size_t unpacked_size = sizeof(char) * len;
    if((ctx->buf_offset + unpacked_size) > ctx->buf_limit){
        return -1;
    }
    memcpy(va_arg(*va, char *), ctx->buf + ctx->buf_offset, unpacked_size);
    ctx->buf_offset += unpacked_size;
    return (ssize_t)unpacked_size;
}

///////////////////////////////////////////////////////////////////////////////
// struct API
///////////////////////////////////////////////////////////////////////////////
const struct struct_format_type format_type_table[255] = {
    [0 ... 254] = { NULL, NULL, 0 },
    ['x'] = { pack_pad, unpack_pad, 1 },
    ['c'] = { pack_uint8, unpack_uint8, 1 },
    ['b'] = { pack_int8, unpack_int8, 1 },
    ['B'] = { pack_uint8, unpack_uint8, 1 },
    ['?'] = { pack_uint8, unpack_uint8, 1 },
    ['h'] = { pack_int16, unpack_int16, 2 },
    ['H'] = { pack_uint16, unpack_uint16, 2 },
    ['i'] = { pack_int32, unpack_int32, 4 },
    ['I'] = { pack_uint32, unpack_uint32, 4 },
    ['l'] = { NULL, NULL, 0 }, //q or i
    ['L'] = { NULL, NULL, 0 }, //Q or I
    ['q'] = { pack_int64, unpack_int64, 8 },
    ['Q'] = { pack_uint64, unpack_uint64, 8 },
    ['f'] = { pack_float, unpack_float, 4 },
    ['d'] = { pack_double, unpack_double, 8 },
    ['s'] = { pack_str, unpack_str, 1 },
    ['p'] = { pack_str, unpack_str, 1 },
    //['P'] = { NULL, NULL, NULL }
};

ssize_t struct_pack(void *buf, size_t buf_limit, const char *format, ...){
    va_list va;
    va_start(va, format);
    if(NULL == buf || 0 >= buf_limit || NULL == format || 0 >= strlen(format)){
        goto err;
    }
    memset(buf, 0, buf_limit);
    struct struct_context ctx = {
        .format = format, .byte_order = BYTE_ORDER, .align = 0,
        .buf = buf, .buf_limit = buf_limit, .buf_offset = 0
    };
    parse_flag(&ctx);
    while(format_has_next(&ctx)){
        int repeat = parse_repeat(&ctx);
        if(0 > repeat){
            goto err;
        }
        char type = parse_type(&ctx);
        if(' ' == type){
            continue;
        }
        struct struct_format_type format_type = format_type_table[(int)type];
        if(!format_type.pack){
            goto err;
        }
        if(0 > format_type.pack(&ctx, &va, repeat)){
            goto err;
        }
    }
    va_end(va);
    return ctx.buf_offset;

err:
    va_end(va);
    return -1;
}

ssize_t struct_unpack(const void *buf, size_t buf_limit, const char *format, ...){
    va_list va;
    va_start(va, format);
    if(NULL == buf || 0 >= buf_limit || NULL == format || 0 >= strlen(format)){
        goto err;
    }
    struct struct_context ctx = {
        .format = format, .byte_order = BYTE_ORDER, .align = 0,
        .buf = (char *)buf, .buf_limit = buf_limit, .buf_offset = 0
    };
    parse_flag(&ctx);
    while(format_has_next(&ctx)){
        int repeat = parse_repeat(&ctx);
        if(0 > repeat){
            goto err;
        }
        char type = parse_type(&ctx);
        if(' ' == type){
            continue;
        }
        struct struct_format_type format_type = format_type_table[(int)type];
        if(!format_type.pack){
            goto err;
        }
        if(0 > format_type.unpack(&ctx, &va, repeat)){
            goto err;
        }
    }
    va_end(va);
    return ctx.buf_offset;

err:
    va_end(va);
    return -1;
}

ssize_t struct_calcsize(const char *format){
    if(NULL == format || 0 >= strlen(format)){
        goto err;
    }
    struct struct_context ctx = {.format = format, .align = 0};
    parse_flag(&ctx);
    ssize_t calcsize = 0;
    while(format_has_next(&ctx)){
        int repeat = parse_repeat(&ctx);
        if(0 > repeat){
            goto err;
        }
        char type = parse_type(&ctx);
        if(' ' == type){
            continue;
        }
        struct struct_format_type format_type = format_type_table[(int)type];
        if(!format_type.pack){
            goto err;
        }
        if(1 < format_type.size){
            if(ctx.align){
                calcsize = CALC_ALIGN(format_type.size, calcsize);
            }
            int i = 0;
            for(; i < repeat; i++){
                calcsize += format_type.size;
            }
        }else{
            calcsize += format_type.size * repeat;
        }
    }
    return calcsize;

err:
    return -1;
}
