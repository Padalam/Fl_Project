# coding=utf8
import ply.lex as lex
import ply.yacc as yacc
import re


class Node:
    def parts_str(self):
        st = []
        for part in self.parts:
            st.append( str( part ) )
        return "\n".join(st)

    def __repr__(self):
        return self.type + ":\n\t" + self.parts_str().replace("\n", "\n\t")
        f.write(self.type + ":\n\t" + self.parts_str().replace("\n", "\n\t"))

    def add_parts(self, parts):
        self.parts += parts
        return self

    def __init__(self, type, parts):
        self.type = type
        self.parts = parts
# без этой штуки ничего не сынтерпретируется, потому что этот массив шарится между лексером и парсером и кроме того используется внутренне библиотекой
tokens = (
    'MINUS',
    'PLUS',
    'MUL_OP',
    'BIN_OP',
    'AND',
    'OR',
    'NOT',
    'FACTOR',
    'NUMBER',
    'IDENT',
    'ASSIGN',
    'L_BRACKET',
    'R_BRACKET',
    'COMMA',
    'L_BRACE',
    'R_BRACE',
    'SEPARATOR',

    #KEYWORDS
    'FUNCTION',
    'IF',
    'ELSE',
    'WHILE',
    "RETURN",
)

# определим регулярку для абстрактного идетификатора
ident = r'[a-z]\w*'
# для каждого токена из массива мы должны написать его определение вида t_ИМЯТОКЕНА = регулярка
t_FUNCTION = r'function'
t_RETURN = r'return'
t_FACTOR = r'\*\*'
t_NOT = r'--'
t_MINUS = r'-'
t_PLUS = r'\+'
t_MUL_OP = r'[*/]'
t_BIN_OP = r'>=|<=|==|<|>|!='
t_AND = r'&&'
t_OR = r'\|\|'
t_NUMBER = r'[1-9]\d*|0'

t_ASSIGN = r'='
t_L_BRACKET = r'\('
t_R_BRACKET = r'\)'
t_COMMA = r','
t_L_BRACE = r'{'
t_R_BRACE = r'}'
t_SEPARATOR = r';'



def t_IDENT(t):
    r'[a-z]\w*'
    if t.value.lower() == 'return':
        t.type = 'RETURN'
    elif t.value.lower() == 'while':
        t.type = 'WHILE'
    elif t.value.lower() == 'if':
        t.type = 'IF'
    elif t.value.lower() == 'else':
        t.type = 'ELSE'
    elif t.value.lower() == 'function':
        t.type = 'FUNCTION'
    return t


# здесь мы игнорируем незначащие символы. Нам ведь все равно, написано $var=$value или $var   =  $value
t_ignore = ' \r\n\t\f'

# а здесь мы обрабатываем ошибки. Кстати заметьте формат названия функции


def t_error(t):
    print("Illegal character '%s' " % t.value[0])
    f.write("Illegal character '%s' " % t.value[0])
    exit(1)


lexer = lex.lex(reflags=re.UNICODE | re.DOTALL)


def p_programm(p):
    '''
        programm : function
                 | function function
                 | function programm
    '''
    if (len(p)== 3):
        p[0] = Node("PROGRAMM",[p[1],p[2]])
    else:
        p[0] = Node("PROGRAMM",[p[1]])
def p_function(p):
    '''
        function : FUNCTION signature L_BRACE body R_BRACE
    '''
    p[0] = Node("FUNCTION",[p[2],p[4]])

def p_signature(p):
    '''
        signature : IDENT L_BRACKET argumentList R_BRACKET
    '''
    p[0] = Node("SIGNATURE",["IDENT " + p[1],p[3]])

def p_body(p):
    '''
    body : infinstruction 

         
    '''

    if (len(p)==2):
        p[0] = Node("BODY",[p[1]])
    if (len(p)>=3):
        p[0] = Node("BODY",[p[1],p[2]])


def p_argumentList(p):

    '''
    argumentList : IDENT
                 | IDENT COMMA argumentList
                 | 
    '''
    if len(p)==2:
        p[0] = p[1]
    else:
            p[0] = Node("ARGUMENTS",[p[1],p[3]])



def p_instruction(p):
    '''
    instruction : assigment 
                | return 
                | condition 
                | loop 
                |  
                | assigment instruction
                | return instruction
                | condition instruction
                | loop instruction
    '''
    if (len(p)==2):
        p[0]= p[1]
    if (len(p)==3):
        p[0] = Node("INSTRUCTION",[p[1]]).add_parts([p[2]])

def p_infinstruction(p):
    '''
    infinstruction : infinstruction instruction 
                   | instruction
    '''
    if len(p)==2:
        p[0] = p[1]
    else:
        p[0]=[2]

def p_assigment(p):
    '''
    assigment : IDENT ASSIGN arExpression
    '''
    p[0] = Node("ASSIGMENT",[p[1],p[3]])

def p_return(p):
    '''
    return : RETURN arExpression
    '''
    p[0] = Node("RETURN",[p[2]])

def p_condition(p):
    '''
    condition : IF arExpression L_BRACE instruction R_BRACE
              | IF arExpression L_BRACE instruction R_BRACE ELSE L_BRACE instruction R_BRACE
    '''
    if (len(p)==6):
        p[0] = Node("IF",[p[2],p[4]])
    else:
        p[0] = Node("IF",[p[2],p[4],"ELSE",p[8]])


def p_loop(p):
    '''
    loop : WHILE arExpression L_BRACE instruction R_BRACE 
    '''
    p[0] = Node("WHILE",[p[2],p[4]])
#-----------------------------------------------
#АРИФМЕТИЧЕСКОЕ ВЫРАЖЕНИЕ
#----------------------------------------------

def p_arExpression(p):
    '''
    arExpression : disjunct 
                 | disjunct OR arExpression
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = Node("OR",[p[1],p[3]])

def p_disjunct(p):
    '''
    disjunct : conjunct
             | conjunct AND disjunct
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = Node("AND",[p[1],p[3]])

def p_conjunct(p):
    '''
    conjunct  : binaryExp
              | binaryExp BIN_OP binaryExp
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = Node(p[2],[p[1],p[3]])
    
def p_binaryExp(p):
    '''
    binaryExp : expression
              | NOT expression
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = Node("NOT",p[2])

def p_expression(p):
    '''
    expression : term
               | expression PLUS term
               | expression MINUS term
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        print(p[1],p[3])
        if (p[1]==p[3] and p[2]=='-'):
            p[0] = "0"
        elif p[3]=='0':
            p[0] = p[1]
        else: 
            p[0] = Node(p[2],[p[1],p[3]])

def p_term(p):
    '''
    term : factor
         | term MUL_OP factor
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        if (p[1]=='1'):
            p[0] = p[3]
        elif (p[3]=='1'):
            p[0] = p[1]
        elif (p[1]=='0' or p[3]=='0'):
            p[0]='0'
        else:
            p[0] = Node(p[2],[p[1],p[3]])

def p_factor(p):
    '''
    factor : base
           | MINUS factor
           | base FACTOR factor
    '''
    if len(p) == 2:
        p[0] = p[1]
    elif len(p) == 3:
        p[0] = Node (p[1],p[2])
    else:
        if p[1]!='0' and p[3]=='0':
            p[0]='1'
        else:
            p[0] = Node(p[2],[p[1],p[3]])

def p_base(p):
    '''
    base : NUMBER
         | IDENT
         | L_BRACKET arExpression R_BRACKET
         | signature
    '''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]
#-----------------------------------------------
#ПРОГРАММА
#----------------------------------------------

def p_error(p):
    print("SYNTAX ERROR AT")
    print(p)

    f.write("SYNTAX ERROR AT: ")
    f.write(str(p))
    exit(1)
f = open('input.txt', 'r')

data = '''
function foo(number)
{
	x = 0*6*7
    y = 5-0
    if (x>3)
    {
        tEst = 3**(2-2)
    }
    return 5-5
}
12312312if()
function test(x,y)
{
    while (condition1 || condition2 && x)
    {
        x = text(x,y) + sin(x)
    }
    return x
}
'''
data = f.read()

lexer.input(data)
f.close()
f = open('output.txt.out', 'w')
parser = yacc.yacc()
while True:
    tok = lexer.token()  # читаем следующий токен
    if not tok:
        break      # закончились печеньки
    #print(tok)

qwe = parser.parse(data)
f.write(str(qwe))


