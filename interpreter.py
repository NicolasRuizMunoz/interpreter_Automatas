import ply.lex as lex   
import ply.yacc as yacc

''' 
Este lenguaje está enfocado principalmente en ayudar a
los gamers para que puedan usar un lenguaje con el que ya están familiarizados
para poder programar e introducirse a las reglas básicas de la programación.
Algunos ejemplos de ejecución:

Python Lenguaje programacion
If -> ranked
else -> normal
elif -> flex
== -> meta
=! -> jg_gap
> -> challenger
< -> hierro
= -> oro
+ -> ganar
- -> perder
False -> Troll
True -> Carry
print -> tab

'''
reserved = {
    'RANKED': 'RANKED',
    'NORMAL': 'NORMAL',
    'FLEX': 'FLEX',
    'META': 'META',
    'JG_GAP': 'JG_GAP',
    'CHALLENGER': 'CHALLENGER',
    'HIERRO': 'HIERRO',
    'ORO': 'ORO',
    'GANAR': 'GANAR',
    'PERDER': 'PERDER',
    'BUILD': 'BUILD',
    'GG': 'GG',
    'FF': 'FF',
    'GO_NEXT': 'GO_NEXT',
    'TROLL': 'TROLL',
    'CARRY': 'CARRY',
    'PUSH': 'PUSH',
    'RUN_IT_DOWN_MID': 'RUN_IT_DOWN_MID',
    'TEAM': 'TEAM',
    'TAB': 'TAB'
}

tokens = [ #revisar bienran
    'ID',
    'NUMBER',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'EQUALS',
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'SEMICOLON',
    'COMMA',
    'COLON',
    'STRING',
    'NEWLINE'
] + list(reserved.values())


# Regular expression rules for simple tokens
t_RANKED = r'ranked' #if
t_NORMAL = r'normal' #else
t_FLEX = r'flex' #elif
t_META = r'meta' #==
t_JG_GAP = r'jg_gap' #!= 
t_CHALLENGER = r'challenger' #>
t_HIERRO = r'hierro' #<
t_ORO = r'oro' #=
t_GANAR = r'ganar' #+
t_PERDER = r'perder' #-
t_BUILD = r'build' #def
t_GG = r'gg' #return
t_FF = r'ff' #break
t_GO_NEXT = r'go next' #continue
t_TROLL = r'troll' #False
t_CARRY = r'carry' #True
t_PUSH = r'push' #For
t_RUN_IT_DOWN_MID = r'run it down mid' #While
t_TEAM = r'team' #List
t_TAB = r'tab' #print


t_ignore  = ' \t'

def t_error(t):
    print(":(")
    t.lexer.skip(1)

lexer = lex.lex()
dato = input()
lexer.input(dato)
while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)