#include "leptjson.h"
#include <assert.h>  /* assert() */
#include <stdlib.h>  /* NULL, strtod() */
#include <errno.h>   /* errno, ERANGE */
#include <math.h>    /* HUGE_VAL */


#define EXPECT(c, ch)       do { assert(*c->json == (ch)); c->json++; } while(0)
#define ISDIGIT(ch)         ((ch) >= '0' && (ch) <= '9')
#define ISDIGIT1TO9(ch)     ((ch) >= '1' && (ch) <= '9')

typedef struct {
    const char* json;
}lept_context;

typedef struct {
    const char* expect_str;
    lept_type expect_type;
}lept_expect_type;

const lept_expect_type LEPT_EXPECT_NULL_TYPE = { "null", LEPT_NULL };
const lept_expect_type LEPT_EXPECT_TRUE_TYPE = { "true", LEPT_TRUE };
const lept_expect_type LEPT_EXPECT_FALSE_TYPE = { "false", LEPT_FALSE };

enum {
    LEPT_NUMBER_START_STATE = 0,
    LEPT_NUMBER_LEADING_NEGAGTIVE_STATE,
    LEPT_NUMBER_LEADING_ZERO_STATE,
    LEPT_NUMBER_LEADING_DIGIT_STATE,
    LEPT_NUMBER_INT_DIGIT_STATE,
    LEPT_NUMBER_DOT_STATE,
    LEPT_NUMBER_FRAC_DIGIT_STATE,
    LEPT_NUMBER_EXP_STATE,
    LEPT_NUMBER_EXP_SIGN_STATE,
    LEPT_NUMBER_EXP_DIGIT_STATE,
};

static void lept_parse_whitespace(lept_context* c) {
    const char *p = c->json;
    while (*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r')
        p++;
    c->json = p;
}

static int lept_parse_literal(lept_context* c, lept_value* v, const lept_expect_type* expect_type){
    assert(expect_type != NULL);
    const char* expect_str_temp = expect_type->expect_str;
    while (*expect_str_temp != '\0')
    {
        if (*c->json == *expect_str_temp)
        {
            c->json++;
            expect_str_temp++;
        }
        else
        {
            return LEPT_PARSE_INVALID_VALUE;
        }
    }
    v->type = expect_type->expect_type;
    return LEPT_PARSE_OK;

}

static int validate_number(lept_context* c)
{
    const char* json_temp = c->json;
    int cur_state = LEPT_NUMBER_START_STATE;
    while (*json_temp != '\0')
    {
        switch (cur_state)
        {
        case LEPT_NUMBER_START_STATE:
            if (*json_temp == '0')
                cur_state = LEPT_NUMBER_LEADING_ZERO_STATE;
            else if (*json_temp == '-')
                cur_state = LEPT_NUMBER_LEADING_NEGAGTIVE_STATE;
            else if (ISDIGIT1TO9(*json_temp))
                cur_state = LEPT_NUMBER_LEADING_DIGIT_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_LEADING_NEGAGTIVE_STATE:
            if (*json_temp == '0')
                cur_state = LEPT_NUMBER_LEADING_ZERO_STATE;
            else if (ISDIGIT1TO9(*json_temp))
                cur_state = LEPT_NUMBER_LEADING_DIGIT_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_LEADING_ZERO_STATE:
            if (*json_temp == 'e' || *json_temp == 'E')
                cur_state = LEPT_NUMBER_EXP_STATE;
            else if (*json_temp == '.')
                cur_state = LEPT_NUMBER_DOT_STATE;
            else
                return LEPT_PARSE_ROOT_NOT_SINGULAR;
            break;
        case LEPT_NUMBER_LEADING_DIGIT_STATE:
            if (*json_temp == '.')
                cur_state = LEPT_NUMBER_DOT_STATE;
            else if (ISDIGIT(*json_temp))
                cur_state = LEPT_NUMBER_INT_DIGIT_STATE;
            else if (*json_temp == 'e' || *json_temp == 'E')
                cur_state = LEPT_NUMBER_EXP_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_INT_DIGIT_STATE:
            if (*json_temp == '.')
                cur_state = LEPT_NUMBER_DOT_STATE;
            else if (ISDIGIT(*json_temp))
                cur_state = LEPT_NUMBER_INT_DIGIT_STATE;
            else if (*json_temp == 'e' || *json_temp == 'E')
                cur_state = LEPT_NUMBER_EXP_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_DOT_STATE:
            if (ISDIGIT(*json_temp))
                cur_state = LEPT_NUMBER_FRAC_DIGIT_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_FRAC_DIGIT_STATE:
            if (ISDIGIT(*json_temp))
                cur_state = LEPT_NUMBER_FRAC_DIGIT_STATE;
            else if (*json_temp == 'e' || *json_temp == 'E')
                cur_state = LEPT_NUMBER_EXP_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_EXP_STATE:
            if (ISDIGIT(*json_temp))
                cur_state = LEPT_NUMBER_EXP_DIGIT_STATE;
            else if (*json_temp == '-' || *json_temp == '+')
                cur_state = LEPT_NUMBER_EXP_SIGN_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_EXP_SIGN_STATE:
            if (ISDIGIT(*json_temp))
                cur_state = LEPT_NUMBER_EXP_DIGIT_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
            break;
        case LEPT_NUMBER_EXP_DIGIT_STATE:
            if (ISDIGIT(*json_temp))
                cur_state = LEPT_NUMBER_EXP_DIGIT_STATE;
            else
                return LEPT_PARSE_INVALID_VALUE;
        default:
            break;
        }
        json_temp++;
    }

    if (cur_state == LEPT_NUMBER_LEADING_ZERO_STATE || cur_state == LEPT_NUMBER_LEADING_DIGIT_STATE ||
        cur_state == LEPT_NUMBER_FRAC_DIGIT_STATE || cur_state == LEPT_NUMBER_EXP_DIGIT_STATE)
        return LEPT_PARSE_OK;
    return LEPT_PARSE_INVALID_VALUE;
}

static int lept_parse_number(lept_context* c, lept_value* v) {
    char* end;
    int validate_number_ret = validate_number(c);
    if (validate_number_ret != LEPT_PARSE_OK)
        return validate_number_ret;
    v->n = strtod(c->json, &end);
    if (errno == ERANGE && (v->n == HUGE_VAL || v->n == -HUGE_VAL))
        return LEPT_PARSE_NUMBER_TOO_BIG;
    if (c->json == end)
        return LEPT_PARSE_INVALID_VALUE;
    c->json = end;
    v->type = LEPT_NUMBER;
    return LEPT_PARSE_OK;
}

static int lept_parse_value(lept_context* c, lept_value* v) {
    switch (*c->json) {
        case 't':  return lept_parse_literal(c, v, &LEPT_EXPECT_TRUE_TYPE);
        case 'f':  return lept_parse_literal(c, v, &LEPT_EXPECT_FALSE_TYPE);
        case 'n':  return lept_parse_literal(c, v, &LEPT_EXPECT_NULL_TYPE);
        default:   return lept_parse_number(c, v);
        case '\0': return LEPT_PARSE_EXPECT_VALUE;
    }
}

int lept_parse(lept_value* v, const char* json) {
    lept_context c;
    int ret;
    assert(v != NULL);
    c.json = json;
    v->type = LEPT_NULL;
    lept_parse_whitespace(&c);
    if ((ret = lept_parse_value(&c, v)) == LEPT_PARSE_OK) {
        lept_parse_whitespace(&c);
        if (*c.json != '\0') {
            v->type = LEPT_NULL;
            ret = LEPT_PARSE_ROOT_NOT_SINGULAR;
        }
    }
    return ret;
}

lept_type lept_get_type(const lept_value* v) {
    assert(v != NULL);
    return v->type;
}

double lept_get_number(const lept_value* v) {
    assert(v != NULL && v->type == LEPT_NUMBER);
    return v->n;
}
