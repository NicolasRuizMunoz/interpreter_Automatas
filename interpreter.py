import ply.lex as lex
import ply.yacc as yacc

import os
import time

global_vars = {}

reserved_keywords = {
    'si' : 'RANKED',
    'entonces' : 'THEN',
    'win' : 'PLUS',
    'lose' : 'MINUS',
    'win_streak' : 'TIMES',
    'lose_streak' : 'DIVIDE',
    'tab' : 'PRINT'
}

token_types = (
    'INT', 'FLOAT',
    'ID', 'NAME', 'LNAME',
    'EQUAL', 'EQUALS', 'NOTEQUALS', 'BIGGER', 'BIGGEROR', 'SMALLER', 'SMALLEROR',
    'LPAREN', 'RPAREN'
) + tuple(reserved_keywords.values())

t_LPAREN  = r"\("
t_RPAREN  = r"\)"
t_PLUS = r'win'
t_MINUS = r'lose'
t_TIMES = r'win_streak'
t_DIVIDE = r'lose_streak'
t_RANKED = r'si'
t_THEN = r'entonces'
t_PRINT = r'tab'
t_EQUAL = r'\='
t_EQUALS = r'\=\='
t_NOTEQUALS = r'\!\='
t_BIGGEROR = r'\<\='
t_BIGGER = r'\<'
t_SMALLEROR = r'\>\='
t_SMALLER = r'\>'

t_ignore = r' '

def t_FLOAT(token):
    r'\d+\.\d+'
    token.value = float(token.value)
    return token

def t_INT(token):
    r'\d+'
    token.value = int(token.value)
    return token

def t_ID(token):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    token.type = reserved_keywords.get(token.value, 'ID')
    return token

def t_LNAME(token):
    r'\'(\s*\w*\s*)*\''
    return token

def t_NAME(token):
    r'\"(\s*\w*\s*)*\"'
    return token

def t_error(token):
    print("\033[1;31;40m   (  Illegal Characters  )\033[0;37;40m")
    token.lexer.skip(1)

lexer = lex.lex()

global_vars = {}
error_counters = {}
error_counters['ErrorCount'] = 0

precedence_rules = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'LPAREN', 'RPAREN'),)

def p_result(p):
    '''
    result : expression
           | if_statement
           | print_statement
           | empty
    '''
    display_result(p[1])

def p_var_assign(p):
    '''
    result : ID EQUAL expression
    '''
    global_vars[p[1]] = p[3]

def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = p[1]

def p_expr_str(p):
    '''
    expression : NAME
               | LNAME
    '''
    p[0] = p[1][1:-1]

def p_expression_var(p):
    '''
    expression : ID
    '''
    try:
        p[0] = global_vars[p[1]]
    except LookupError:
        print('')
        print("\033[1;33;40m   (  Variable indefinida '%s'  )\033[0;37;40m" % p[1])
        print("┐(ﾟ ～ﾟ )┌")
        time.sleep(1)
        p[0] = None

def p_expression_operations(p):
    '''
    expression : expression TIMES expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
    '''
    if p[2] == 'win':
        p[0] = p[1] + p[3]
        if error_counters['ErrorCount'] > 0:
            error_counters['ErrorCount'] -= 1
    elif p[2] == 'lose':
        p[0] = p[1] - p[3]
        if error_counters['ErrorCount'] > 0:
            error_counters['ErrorCount'] -= 1
    elif p[2] == 'win_streak':
        p[0] = p[1] * p[3]
        if error_counters['ErrorCount'] > 0:
            error_counters['ErrorCount'] -= 1
    elif p[2] == 'lose_streak':
        try:
            p[0] = p[1] / p[3]
            if error_counters['ErrorCount'] > 0:
                error_counters['ErrorCount'] -= 1
        except ZeroDivisionError:
            print("")
            print("\033[1;33;40m   (  Can't divide by 0  )\033[0;37;40m")
            print("(ﾉﾟοﾟ)ﾉ")
            time.sleep(1)
            p[0] = None

def p_print_statement(p):
    '''
    print_statement : PRINT LPAREN expression RPAREN
    '''
    print(p[3])

def p_if_statement(p):
    '''
    if_statement : RANKED comparison THEN expression
                 | RANKED comparison THEN print_statement
    '''
    if p[2]:
        p[0] = p[4]
    else:
        p[0] = None

def p_comparison(p):
    '''
    comparison : expression EQUALS expression
               | expression NOTEQUALS expression
               | expression BIGGER expression
               | expression BIGGEROR expression
               | expression SMALLER expression
               | expression SMALLEROR expression
    '''
    if p[2] == '==':
        p[0] = p[1] == p[3]
    elif p[2] == '!=':
        p[0] = p[1] != p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>=':
        p[0] = p[1] >= p[3]
    elif p[2] == '<=':
        p[0] = p[1] <= p[3]

def p_factor_paren(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    p[0] = p[2]

def p_empty(p):
    '''
    empty : 
    '''
    p[0] = None

def p_error(p):
    print("")
    print("\033[1;31;40m   (  Syntax Error!  )\033[0;37;40m")
    print("(╬≖_≖)")
    time.sleep(1)
    error_counters['ErrorCount'] += 1

def display_result(result):
    if error_counters['ErrorCount'] > 15:
        print("\033[1;37;40m     Te equivocaste mucho, no te quiero contestar!\033[0;37;40m")
        print("٩ (╬ʘ益ʘ╬) ۶")
        time.sleep(1)
        return
    if result is not None:
        print("          ", result)

def prompt_message():
    if error_counters['ErrorCount'] == 6:
        print("           _/==+\ Reglas del Lenguaje /+==\_\n",
              '            "   "  =================  "   "  ')
        print("---------------------------------------------------------")
        print("                      Suma = 'win'\n",
              "                    Resta = 'lose'\n",
              "                 Multiplicacion = 'win_streak'\n",
              "                  Division = 'lose_streak'\n")
        print("---------------------------------------------------------")
        error_counters['ErrorCount'] += 1
        return input('\033[1;47;40m(ﾉﾟοﾟ)ﾉ\033[0;37;40m')

    if error_counters['ErrorCount'] == 10:
        print("No se si me estas tratando enojar o que?")
        error_counters['ErrorCount'] += 1
        return input('\033[1;47;40m(￣ｰ￣)\033[0;37;40m')

    if error_counters['ErrorCount'] < 7:
        return input('\033[1;47;40m( /・・)ノ\033[0;37;40m')
    elif error_counters['ErrorCount'] > 6 and error_counters['ErrorCount'] < 10:
        return input('\033[1;47;40m(¬▂¬)\033[0;37;40m')
    elif error_counters['ErrorCount'] > 10:
        return input('\033[1;47;40m(╬≖_≖)\033[0;37;40m')

parser = yacc.yacc()

os.system('CLS')

while True:
    try:
        user_input = prompt_message()
        os.system('CLS')
    except EOFError:
        break
    parser.parse(user_input)
