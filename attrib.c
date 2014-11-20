#include <assert.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#include "attrib.h"

static int 
op_preced(const char c)
{
	switch(c)    {
        case '^':
            return 4;
		case '*':  case '/': case '%':
			return 3;
		case '+': case '-':
			return 2;
		case '=':
			return 1;
	}
	return 0;
}

static bool 
op_left_assoc(const char c)
{
	switch(c)    {
		// left to right
        case '*': case '/': case '%': case '+': case '-': case '^': 
			return true;
			// right to left
		case '=':
			return false;
	}
	return false;
}


#define is_operator(c)  (c == '+' || c == '-' || c == '/' || c == '*' || c == '%' || c == '=' || c == '^')
#define is_ident(c)     (c != '(' && c != ')' && !is_operator(c))

static bool 
shunting_yard(const char *input, char *output, char  **err)
{
 	const char *strpos = input, *strend = input + strlen(input);
	char c, *outpos = output;

	char stack[32];       // operator stack
	unsigned int sl = 0;  // stack length
	char     sc;          // used for record stack element

	while(strpos < strend)   {
		// read one token from the input stream
		c = *strpos;
		if(!isspace(c))    {
			// If the token is a number (identifier), then add it to the output queue.
			if(is_ident(c))  {
				while(is_ident(*strpos) && strpos < strend){
					*outpos = *strpos; ++outpos;
					strpos++;
				}
				*outpos = ' '; 
				++outpos;
				--strpos;
			}
			// If the token is an operator, op1, then:
			else if(is_operator(c))  {
				while(sl > 0)    {
					sc = stack[sl - 1];
					// While there is an operator token, op2, at the top of the stack
					// op1 is left-associative and its precedence is less than or equal to that of op2,
					// or op1 has precedence less than that of op2,
					// Let + and ^ be right associative.
					// Correct transformation from 1^2+3 is 12^3+
					// The differing operator priority decides pop / push
					// If 2 operators have equal priority then associativity decides.
					if(is_operator(sc) &&
						((op_left_assoc(c) && (op_preced(c) <= op_preced(sc))) ||
						(op_preced(c) < op_preced(sc))))   {
							// Pop op2 off the stack, onto the output queue;
							*outpos = sc; 
							++outpos;
							*outpos = ' '; 
							++outpos;
							sl--;
						}
					else   {
						break;
					}
				}
				// push op1 onto the stack.
				stack[sl] = c;
				++sl;
			}
			// If the token is a left parenthesis, then push it onto the stack.
			else if(c == '(')   {
				stack[sl] = c;
				++sl;
			}
			// If the token is a right parenthesis:
			else if(c == ')')    {
				bool pe = false;
				// Until the token at the top of the stack is a left parenthesis,
				// pop operators off the stack onto the output queue
				while(sl > 0)     {
					sc = stack[sl - 1];
					if(sc == '(')    {
						pe = true;
						break;
					}
					else  {
						*outpos = sc; 
						++outpos;
						*outpos = ' '; 
						++outpos;						
						sl--;
					}
				}
				// If the stack runs out without finding a left parenthesis, then there are mismatched parentheses.
				if(!pe)  {
					*err = "Error: parentheses mismatched\n";
					return false;
				}
				// Pop the left parenthesis from the stack, but not onto the output queue.
				sl--;
			}
			else  {
				*err = "Unknown token \n";
				return false; // Unknown token
			}
		}
		++strpos;
	}
	// When there are no more tokens to read:
	// While there are still operator tokens in the stack:
	while(sl > 0)  {
		sc = stack[sl - 1];
		if(sc == '(' || sc == ')')   {
			*err = "Error: parentheses mismatched\n";
			return false;
		}
		*outpos = sc; 
		++outpos;
		*outpos = ' '; 
		++outpos;
		--sl;
	}
	*outpos = 0; // Null terminator
	return true;
}


#define TOKEN_LEN 32
#define TOKEN_SIZE 4
#define VALUE_SIZE 2

typedef enum {
    T_OPERATOR,
    T_REGISTER,
    T_CONST
} ValueType;

typedef struct Token{
    unsigned int hash_value;
    int register_index;
    size_t len;
    char str[TOKEN_LEN + 1];
    struct Token *next;
} Token;


typedef struct Value{
    ValueType type;
    union{
        char operator;
        int register_index;
        float const_value;
    } info;
} Value;

typedef struct Expression{
    struct Expression *next;
    char *dump_string;
    struct Value *value_array;
    int value_size;
} Expression;

struct Expressions{
    Expression *first;
    Expression *end;
    Token ** token_list;
    int token_size;
    int token_use_size;
    int stack_size;
    int register_size;
    int dirty;
};

typedef struct StackValue{
    float value;
    int register_index;
} StackValue;

struct Attrib{
    float *reg;
    struct StackValue *stack;
    Expressions *exps;
    int dirty;
};


static unsigned int 
hash(const char *string){

	unsigned int l = strlen(string);
	unsigned int h = l;
	size_t step = (l>>5)+1;
	size_t ll;

	for(ll=1; ll>=step; ll-=step){
		h = h ^ ((h<<5)+(h>>2) + (unsigned char)(string[ll-1]));
	}
	return h;
}


static int
find_token(Expressions *exps, const char *token_name, int *register_pos){
    unsigned int HashValue = hash(token_name);
    size_t token_len = strlen(token_name);
    int index = HashValue % exps->token_size;
    Token *token = exps->token_list[index];
    while(token){
        Token *next = token->next;
        if(token->hash_value == HashValue && token_len == token->len && memcmp(token_name, token->str, token_len) == 0){
            *register_pos = token->register_index;
            return 1;
        }else{
            token = next;
        }

    }
    return 0;
}


Expressions *
exps_new(){
    Expressions *exps = malloc(sizeof(Expressions));
    memset(exps, 0, sizeof(Expressions));
    exps->token_list = malloc(sizeof(Token*) * TOKEN_SIZE);
    memset(exps->token_list, 0, sizeof(Token*) * TOKEN_SIZE);
    exps->token_size = TOKEN_SIZE;
    return exps;
}

int 
exps_epush(Expressions *exps, const char *formula, char **err){
    char output[2048];
    if(!shunting_yard(formula, output, err)){
        return 0;
    }

    Expression *exp = malloc(sizeof(Expression));
    memset(exp, 0, sizeof(Expression));
    int value_size = VALUE_SIZE;
    int value_use_size = 0;
    exp->value_array = malloc(value_size * sizeof(Value));

    size_t output_len = strlen(output);
    exp->dump_string = malloc((output_len + 1)*sizeof(char));
    strcpy(exp->dump_string, output);
    exp->dump_string[output_len] = 0;

    char *str = exp->dump_string;

    while(*str){
        if(*str == ' '){
            str ++;
        }else{
            value_use_size ++;
            
            if(value_use_size > value_size){
                value_size *= 2;
                Value *new_value_array = realloc(exp->value_array, value_size * sizeof(Value));
                exp->value_array = new_value_array;
            }
            Value *value =  exp->value_array + value_use_size - 1;
            if(is_operator(*str)){
                value->type = T_OPERATOR;
                value->info.operator = *str;
                str ++;
            }else if( '0'<= *str && *str <= '9'){
                char *end;
                value->type = T_CONST;
                value->info.const_value = strtof(str, &end);
                str = end;
            }else{ // is token stack[stack_pos - 2]
                char token_name[TOKEN_LEN+1];
                int j = 0;
                while(*str != ' ' && *str != 0){
                    token_name[j++] = *(str++);
                    if(j > TOKEN_LEN){
                        *err = "token is too long(>TOKEN_LEN)";
                        return 0;
                    }
                }
                token_name[j] = 0;
                int register_index;
                value->type = T_REGISTER;
                if(find_token(exps, token_name, &register_index)){
                    value->info.register_index = register_index;
                }else{
                    value->info.register_index = exps->token_use_size;
                    exps->token_use_size ++;
                    if(exps->token_use_size > exps->token_size){
                        int old_size = exps->token_size;
                        exps->token_size *= 2;
                        Token **token_list = malloc(sizeof(Token*) * exps->token_size);
                        memset(token_list, 0, sizeof(Token*) * exps->token_size);
                        for(int i = 0; i < old_size; i++){
                            Token *token = exps->token_list[i];
                            while(token){
                                int index = token->hash_value % exps->token_size;
                                Token *old_token = token_list[index];
                                token_list[index] = token;
                                Token *token_next = token->next;
                                token->next = old_token;
                                token = token_next;
                            }
                        }
                        free(exps->token_list);
                        exps->token_list = token_list;
                    }
                    Token *new_token = malloc(sizeof(Token));
                    new_token->hash_value = hash(token_name);
                    new_token->register_index = exps->register_size;
                    exps->register_size ++;
                    size_t token_len = strlen(token_name);
                    new_token->len = token_len;
                    strcpy(new_token->str, token_name);
                    int index = new_token->hash_value % exps->token_size;
                    Token *old_token = exps->token_list[index];
                    exps->token_list[index] = new_token;
                    new_token->next = old_token;
                }
            }
        }
    }

    exp->value_size = value_use_size;
    exp->value_array = realloc(exp->value_array, exp->value_size * sizeof(Value));

    int stack_size = 0, max_stack_size = 0;
    for(int j = 0; j < exp->value_size; j++){
        if(exp->value_array[j].type == T_OPERATOR){
           stack_size -= 1; 
        }else{
            stack_size ++;
            max_stack_size = stack_size > max_stack_size ? stack_size : max_stack_size;
        }
    }
    exps->stack_size = exps->stack_size > max_stack_size ? exps->stack_size : max_stack_size;

    if(exps->first == 0){
        exps->first = exp;
        exps->end = exp;
    }else{
        exps->end->next = exp;
        exps->end = exp;
    }
    return 1;
}

void
exps_delete(Expressions *exps){
    free(exps->token_list);
    Expression *exp = exps->first;
    while(exp){
        free(exp->dump_string);
        free(exp->value_array);
        Expression *next = exp->next;
        free(exp);
        exp = next;
    }
    free(exps);
}


Attrib *
attrib_new(Expressions *exps){
    Attrib *attrib = malloc(sizeof(Attrib));
    attrib->reg = malloc(sizeof(float) * exps->register_size);
    memset(attrib->reg, 0, sizeof(float) * exps->register_size);
    attrib->stack = malloc(sizeof(StackValue) * exps->stack_size);
    attrib->exps = exps;

    return attrib;
}


static int
_calc(Attrib *attrib, char **err){
    Expressions *exps = attrib->exps;
    Expression *exp = exps->first;
    StackValue *stack = attrib->stack;
    float *reg = attrib->reg;
    while(exp){
        int stack_pos = 0;
        for(int i = 0; i < exp->value_size; i++){
            Value *value = exp->value_array + i;
            switch(value->type) {
                case T_CONST:
                    assert(stack_pos < exps->stack_size);
                    stack[stack_pos].value = value->info.const_value;
                    stack[stack_pos].register_index = -1;
                    stack_pos++;
                    break;
                case T_REGISTER:
                    assert(stack_pos < exps->stack_size);
                    assert(value->info.register_index < exps->register_size);
                    stack[stack_pos].value = reg[value->info.register_index];
                    stack[stack_pos].register_index = value->info.register_index;
                    stack_pos ++;
                    break;
                case T_OPERATOR:
                    assert(stack_pos >= 2);
                    switch(value->info.operator) {
                        case '+':
                            stack[stack_pos - 2].value = stack[stack_pos - 2].value + stack[stack_pos - 1].value;
                            break;
                        case '-':
                            stack[stack_pos - 2].value = stack[stack_pos - 2].value - stack[stack_pos - 1].value;
                            break;
                        case '*':
                            stack[stack_pos - 2].value = stack[stack_pos - 2].value * stack[stack_pos - 1].value;
                            break;
                        case '/':
                            stack[stack_pos - 2].value = stack[stack_pos - 2].value / stack[stack_pos - 1].value;
                            break;
                        case '%':
                            stack[stack_pos - 2].value = (int)stack[stack_pos - 2].value % (int)stack[stack_pos - 1].value;
                            break;
                        case '^':
                            stack[stack_pos - 2].value = pow(stack[stack_pos - 2].value, stack[stack_pos - 1].value);
                            break;
                        case '=':
                            stack[stack_pos - 2].value = stack[stack_pos - 1].value;
                            assert(stack[stack_pos - 2].register_index >= 0);
                            reg[stack[stack_pos - 2].register_index] = stack[stack_pos - 2].value;
                            break;
                        default:
                            *err = "unknown operator";
                            return 0;
                    }
                    stack_pos--;
                    break;
                default:
                    *err = "unknown value type";
                    return 0;
            }
        }
        exp = exp->next;
    }
    return 1;
}

void
attrib_delete(Attrib *attrib){
    free(attrib->reg);
    free(attrib->stack);
    free(attrib);
}

int
attrib_set(Attrib *attrib, const char *name, float value, char **err){
    Expressions *exps = attrib->exps;
    int register_pos;
    if(find_token(exps, name, &register_pos)){
        assert(register_pos < exps->register_size);
        attrib->reg[register_pos] = value;
        attrib->dirty = 1;
        return 1;
    }else{
        *err = "donnot have this token";
        return 0;
    }
}

int
attrib_get(Attrib *attrib, const char *name, float *value, char **err){
    Expressions *exps = attrib->exps;
    int register_pos;
    if(find_token(exps, name, &register_pos)){
        assert(register_pos < exps->register_size);
        if(attrib->dirty){
            if(_calc(attrib, err)){
                attrib->dirty = 0;
            }else{
                return 0;
            }
        }
        *value = attrib->reg[register_pos];
        return 1;
    }else{
        *err = "donnot have this token";
        return 0;
    }
}

