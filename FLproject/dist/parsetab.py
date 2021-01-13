
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'AND ASSIGN BIN_OP COMMA ELSE FACTOR FUNCTION IDENT IF L_BRACE L_BRACKET MINUS MUL_OP NOT NUMBER OR PLUS RETURN R_BRACE R_BRACKET SEPARATOR WHILE\n        programm : function\n                 | function function\n                 | function programm\n    \n        function : FUNCTION signature L_BRACE body R_BRACE\n    \n        signature : IDENT L_BRACKET argumentList R_BRACKET\n    \n    body : infinstruction \n\n         \n    \n    argumentList : IDENT\n                 | IDENT COMMA argumentList\n                 | \n    \n    instruction : assigment \n                | return \n                | condition \n                | loop \n                |  \n                | assigment instruction\n                | return instruction\n                | condition instruction\n                | loop instruction\n    \n    infinstruction : infinstruction instruction \n                   | instruction\n    \n    assigment : IDENT ASSIGN arExpression\n    \n    return : RETURN arExpression\n    \n    condition : IF arExpression L_BRACE instruction R_BRACE\n              | IF arExpression L_BRACE instruction R_BRACE ELSE L_BRACE instruction R_BRACE\n    \n    loop : WHILE arExpression L_BRACE instruction R_BRACE \n    \n    arExpression : disjunct \n                 | disjunct OR arExpression\n    \n    disjunct : conjunct\n             | conjunct AND disjunct\n    \n    conjunct  : binaryExp\n              | binaryExp BIN_OP binaryExp\n    \n    binaryExp : expression\n              | NOT expression\n    \n    expression : term\n               | expression PLUS term\n               | expression MINUS term\n    \n    term : factor\n         | term MUL_OP factor\n    \n    factor : base\n           | MINUS factor\n           | base FACTOR factor\n    \n    base : NUMBER\n         | IDENT\n         | L_BRACKET arExpression R_BRACKET\n         | signature\n    '
    
_lr_action_items = {'FUNCTION':([0,2,4,23,],[3,3,3,-4,]),'$end':([1,2,4,5,23,],[0,-1,-1,-3,-4,]),'IDENT':([3,8,9,11,12,13,14,15,16,18,19,20,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,46,47,48,49,50,51,52,53,54,55,56,57,59,60,62,63,64,65,66,67,68,69,72,73,75,77,],[7,17,21,17,-20,17,17,17,17,41,41,41,-19,-15,-16,-17,-18,41,-22,-26,-28,-30,-32,41,-34,41,-37,-39,-42,-43,41,-45,21,-5,-21,41,41,41,41,41,-33,41,-40,41,17,17,-27,-29,-31,-35,-36,-38,-41,-44,-23,-25,17,-24,]),'L_BRACE':([6,31,32,33,34,36,38,39,40,41,43,44,45,47,54,56,62,63,64,65,66,67,68,69,74,],[8,-26,-28,-30,-32,-34,-37,-39,-42,-43,-45,59,60,-5,-33,-40,-27,-29,-31,-35,-36,-38,-41,-44,75,]),'L_BRACKET':([7,18,19,20,29,35,37,41,42,49,50,51,52,53,55,57,],[9,42,42,42,42,42,42,9,42,42,42,42,42,42,42,42,]),'RETURN':([8,11,12,13,14,15,16,24,25,26,27,28,30,31,32,33,34,36,38,39,40,41,43,47,48,54,56,59,60,62,63,64,65,66,67,68,69,72,73,75,77,],[18,18,-20,18,18,18,18,-19,-15,-16,-17,-18,-22,-26,-28,-30,-32,-34,-37,-39,-42,-43,-45,-5,-21,-33,-40,18,18,-27,-29,-31,-35,-36,-38,-41,-44,-23,-25,18,-24,]),'IF':([8,11,12,13,14,15,16,24,25,26,27,28,30,31,32,33,34,36,38,39,40,41,43,47,48,54,56,59,60,62,63,64,65,66,67,68,69,72,73,75,77,],[19,19,-20,19,19,19,19,-19,-15,-16,-17,-18,-22,-26,-28,-30,-32,-34,-37,-39,-42,-43,-45,-5,-21,-33,-40,19,19,-27,-29,-31,-35,-36,-38,-41,-44,-23,-25,19,-24,]),'WHILE':([8,11,12,13,14,15,16,24,25,26,27,28,30,31,32,33,34,36,38,39,40,41,43,47,48,54,56,59,60,62,63,64,65,66,67,68,69,72,73,75,77,],[20,20,-20,20,20,20,20,-19,-15,-16,-17,-18,-22,-26,-28,-30,-32,-34,-37,-39,-42,-43,-45,-5,-21,-33,-40,20,20,-27,-29,-31,-35,-36,-38,-41,-44,-23,-25,20,-24,]),'R_BRACE':([8,10,11,12,13,14,15,16,24,25,26,27,28,30,31,32,33,34,36,38,39,40,41,43,47,48,54,56,59,60,62,63,64,65,66,67,68,69,70,71,72,73,75,76,77,],[-14,23,-6,-20,-10,-11,-12,-13,-19,-15,-16,-17,-18,-22,-26,-28,-30,-32,-34,-37,-39,-42,-43,-45,-5,-21,-33,-40,-14,-14,-27,-29,-31,-35,-36,-38,-41,-44,72,73,-23,-25,-14,77,-24,]),'R_BRACKET':([9,21,22,31,32,33,34,36,38,39,40,41,43,46,47,54,56,58,61,62,63,64,65,66,67,68,69,],[-9,-7,47,-26,-28,-30,-32,-34,-37,-39,-42,-43,-45,-9,-5,-33,-40,69,-8,-27,-29,-31,-35,-36,-38,-41,-44,]),'ASSIGN':([17,],[29,]),'NOT':([18,19,20,29,42,49,50,51,],[35,35,35,35,35,35,35,35,]),'MINUS':([18,19,20,29,34,35,36,37,38,39,40,41,42,43,47,49,50,51,52,53,54,55,56,57,65,66,67,68,69,],[37,37,37,37,53,37,-34,37,-37,-39,-42,-43,37,-45,-5,37,37,37,37,37,53,37,-40,37,-35,-36,-38,-41,-44,]),'NUMBER':([18,19,20,29,35,37,42,49,50,51,52,53,55,57,],[40,40,40,40,40,40,40,40,40,40,40,40,40,40,]),'COMMA':([21,],[46,]),'OR':([31,32,33,34,36,38,39,40,41,43,47,54,56,63,64,65,66,67,68,69,],[49,-28,-30,-32,-34,-37,-39,-42,-43,-45,-5,-33,-40,-29,-31,-35,-36,-38,-41,-44,]),'AND':([32,33,34,36,38,39,40,41,43,47,54,56,64,65,66,67,68,69,],[50,-30,-32,-34,-37,-39,-42,-43,-45,-5,-33,-40,-31,-35,-36,-38,-41,-44,]),'BIN_OP':([33,34,36,38,39,40,41,43,47,54,56,65,66,67,68,69,],[51,-32,-34,-37,-39,-42,-43,-45,-5,-33,-40,-35,-36,-38,-41,-44,]),'PLUS':([34,36,38,39,40,41,43,47,54,56,65,66,67,68,69,],[52,-34,-37,-39,-42,-43,-45,-5,52,-40,-35,-36,-38,-41,-44,]),'MUL_OP':([36,38,39,40,41,43,47,56,65,66,67,68,69,],[55,-37,-39,-42,-43,-45,-5,-40,55,55,-38,-41,-44,]),'FACTOR':([39,40,41,43,47,69,],[57,-42,-43,-45,-5,-44,]),'ELSE':([72,],[74,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'programm':([0,2,4,],[1,5,5,]),'function':([0,2,4,],[2,4,4,]),'signature':([3,18,19,20,29,35,37,42,49,50,51,52,53,55,57,],[6,43,43,43,43,43,43,43,43,43,43,43,43,43,43,]),'body':([8,],[10,]),'infinstruction':([8,],[11,]),'instruction':([8,11,13,14,15,16,59,60,75,],[12,24,25,26,27,28,70,71,76,]),'assigment':([8,11,13,14,15,16,59,60,75,],[13,13,13,13,13,13,13,13,13,]),'return':([8,11,13,14,15,16,59,60,75,],[14,14,14,14,14,14,14,14,14,]),'condition':([8,11,13,14,15,16,59,60,75,],[15,15,15,15,15,15,15,15,15,]),'loop':([8,11,13,14,15,16,59,60,75,],[16,16,16,16,16,16,16,16,16,]),'argumentList':([9,46,],[22,61,]),'arExpression':([18,19,20,29,42,49,],[30,44,45,48,58,62,]),'disjunct':([18,19,20,29,42,49,50,],[31,31,31,31,31,31,63,]),'conjunct':([18,19,20,29,42,49,50,],[32,32,32,32,32,32,32,]),'binaryExp':([18,19,20,29,42,49,50,51,],[33,33,33,33,33,33,33,64,]),'expression':([18,19,20,29,35,42,49,50,51,],[34,34,34,34,54,34,34,34,34,]),'term':([18,19,20,29,35,42,49,50,51,52,53,],[36,36,36,36,36,36,36,36,36,65,66,]),'factor':([18,19,20,29,35,37,42,49,50,51,52,53,55,57,],[38,38,38,38,38,56,38,38,38,38,38,38,67,68,]),'base':([18,19,20,29,35,37,42,49,50,51,52,53,55,57,],[39,39,39,39,39,39,39,39,39,39,39,39,39,39,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> programm","S'",1,None,None,None),
  ('programm -> function','programm',1,'p_programm','lexer.py',110),
  ('programm -> function function','programm',2,'p_programm','lexer.py',111),
  ('programm -> function programm','programm',2,'p_programm','lexer.py',112),
  ('function -> FUNCTION signature L_BRACE body R_BRACE','function',5,'p_function','lexer.py',120),
  ('signature -> IDENT L_BRACKET argumentList R_BRACKET','signature',4,'p_signature','lexer.py',126),
  ('body -> infinstruction','body',1,'p_body','lexer.py',132),
  ('argumentList -> IDENT','argumentList',1,'p_argumentList','lexer.py',145),
  ('argumentList -> IDENT COMMA argumentList','argumentList',3,'p_argumentList','lexer.py',146),
  ('argumentList -> <empty>','argumentList',0,'p_argumentList','lexer.py',147),
  ('instruction -> assigment','instruction',1,'p_instruction','lexer.py',159),
  ('instruction -> return','instruction',1,'p_instruction','lexer.py',160),
  ('instruction -> condition','instruction',1,'p_instruction','lexer.py',161),
  ('instruction -> loop','instruction',1,'p_instruction','lexer.py',162),
  ('instruction -> <empty>','instruction',0,'p_instruction','lexer.py',163),
  ('instruction -> assigment instruction','instruction',2,'p_instruction','lexer.py',164),
  ('instruction -> return instruction','instruction',2,'p_instruction','lexer.py',165),
  ('instruction -> condition instruction','instruction',2,'p_instruction','lexer.py',166),
  ('instruction -> loop instruction','instruction',2,'p_instruction','lexer.py',167),
  ('infinstruction -> infinstruction instruction','infinstruction',2,'p_infinstruction','lexer.py',176),
  ('infinstruction -> instruction','infinstruction',1,'p_infinstruction','lexer.py',177),
  ('assigment -> IDENT ASSIGN arExpression','assigment',3,'p_assigment','lexer.py',186),
  ('return -> RETURN arExpression','return',2,'p_return','lexer.py',192),
  ('condition -> IF arExpression L_BRACE instruction R_BRACE','condition',5,'p_condition','lexer.py',198),
  ('condition -> IF arExpression L_BRACE instruction R_BRACE ELSE L_BRACE instruction R_BRACE','condition',9,'p_condition','lexer.py',199),
  ('loop -> WHILE arExpression L_BRACE instruction R_BRACE','loop',5,'p_loop','lexer.py',209),
  ('arExpression -> disjunct','arExpression',1,'p_arExpression','lexer.py',218),
  ('arExpression -> disjunct OR arExpression','arExpression',3,'p_arExpression','lexer.py',219),
  ('disjunct -> conjunct','disjunct',1,'p_disjunct','lexer.py',228),
  ('disjunct -> conjunct AND disjunct','disjunct',3,'p_disjunct','lexer.py',229),
  ('conjunct -> binaryExp','conjunct',1,'p_conjunct','lexer.py',238),
  ('conjunct -> binaryExp BIN_OP binaryExp','conjunct',3,'p_conjunct','lexer.py',239),
  ('binaryExp -> expression','binaryExp',1,'p_binaryExp','lexer.py',248),
  ('binaryExp -> NOT expression','binaryExp',2,'p_binaryExp','lexer.py',249),
  ('expression -> term','expression',1,'p_expression','lexer.py',258),
  ('expression -> expression PLUS term','expression',3,'p_expression','lexer.py',259),
  ('expression -> expression MINUS term','expression',3,'p_expression','lexer.py',260),
  ('term -> factor','term',1,'p_term','lexer.py',275),
  ('term -> term MUL_OP factor','term',3,'p_term','lexer.py',276),
  ('factor -> base','factor',1,'p_factor','lexer.py',292),
  ('factor -> MINUS factor','factor',2,'p_factor','lexer.py',293),
  ('factor -> base FACTOR factor','factor',3,'p_factor','lexer.py',294),
  ('base -> NUMBER','base',1,'p_base','lexer.py',308),
  ('base -> IDENT','base',1,'p_base','lexer.py',309),
  ('base -> L_BRACKET arExpression R_BRACKET','base',3,'p_base','lexer.py',310),
  ('base -> signature','base',1,'p_base','lexer.py',311),
]