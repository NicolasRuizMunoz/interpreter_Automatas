
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'BUILD CARRY CHALLENGER COLON COMMA DIVIDE EQUALS FF FLEX GANAR GG GO_NEXT HIERRO ID JG_GAP LBRACKET LPAREN META MINUS NEWLINE NORMAL NUMBER ORO PERDER PLUS PUSH RANKED RBRACKET RPAREN RUN_IT_DOWN_MID SEMICOLON STRING TAB TEAM TIMES TROLLprogram : statementsstatements : statements statement\n                  | statementstatement : if_statement\n                 | else_statement\n                 | print_statement\n                 | assignment_statement\n                 | function_definitionif_statement : RANKED ORO expression META expression COLON statementselse_statement : NORMAL COLON statementsprint_statement : TAB STRING SEMICOLONassignment_statement : ID ORO expression SEMICOLONfunction_definition : BUILD ID COLON statementsexpression : NUMBER\n                  | STRING\n                  | ID'
    
_lr_action_items = {'RANKED':([0,2,3,4,5,6,7,8,14,16,24,25,27,29,30,32,33,],[9,9,-3,-4,-5,-6,-7,-8,-2,9,9,-11,9,-12,9,9,9,]),'NORMAL':([0,2,3,4,5,6,7,8,14,16,24,25,27,29,30,32,33,],[10,10,-3,-4,-5,-6,-7,-8,-2,10,10,-11,10,-12,10,10,10,]),'TAB':([0,2,3,4,5,6,7,8,14,16,24,25,27,29,30,32,33,],[11,11,-3,-4,-5,-6,-7,-8,-2,11,11,-11,11,-12,11,11,11,]),'ID':([0,2,3,4,5,6,7,8,13,14,15,16,18,24,25,27,28,29,30,32,33,],[12,12,-3,-4,-5,-6,-7,-8,19,-2,23,12,23,12,-11,12,23,-12,12,12,12,]),'BUILD':([0,2,3,4,5,6,7,8,14,16,24,25,27,29,30,32,33,],[13,13,-3,-4,-5,-6,-7,-8,-2,13,13,-11,13,-12,13,13,13,]),'$end':([1,2,3,4,5,6,7,8,14,24,25,29,30,33,],[0,-1,-3,-4,-5,-6,-7,-8,-2,-10,-11,-12,-13,-9,]),'ORO':([9,12,],[15,18,]),'COLON':([10,19,21,22,23,31,],[16,27,-14,-15,-16,32,]),'STRING':([11,15,18,28,],[17,22,22,22,]),'NUMBER':([15,18,28,],[21,21,21,]),'SEMICOLON':([17,21,22,23,26,],[25,-14,-15,-16,29,]),'META':([20,21,22,23,],[28,-14,-15,-16,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'statements':([0,16,27,32,],[2,24,30,33,]),'statement':([0,2,16,24,27,30,32,33,],[3,14,3,14,3,14,3,14,]),'if_statement':([0,2,16,24,27,30,32,33,],[4,4,4,4,4,4,4,4,]),'else_statement':([0,2,16,24,27,30,32,33,],[5,5,5,5,5,5,5,5,]),'print_statement':([0,2,16,24,27,30,32,33,],[6,6,6,6,6,6,6,6,]),'assignment_statement':([0,2,16,24,27,30,32,33,],[7,7,7,7,7,7,7,7,]),'function_definition':([0,2,16,24,27,30,32,33,],[8,8,8,8,8,8,8,8,]),'expression':([15,18,28,],[20,26,31,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> statements','program',1,'p_program','interpreter.py',107),
  ('statements -> statements statement','statements',2,'p_statements','interpreter.py',111),
  ('statements -> statement','statements',1,'p_statements','interpreter.py',112),
  ('statement -> if_statement','statement',1,'p_statement','interpreter.py',119),
  ('statement -> else_statement','statement',1,'p_statement','interpreter.py',120),
  ('statement -> print_statement','statement',1,'p_statement','interpreter.py',121),
  ('statement -> assignment_statement','statement',1,'p_statement','interpreter.py',122),
  ('statement -> function_definition','statement',1,'p_statement','interpreter.py',123),
  ('if_statement -> RANKED ORO expression META expression COLON statements','if_statement',7,'p_if_statement','interpreter.py',127),
  ('else_statement -> NORMAL COLON statements','else_statement',3,'p_else_statement','interpreter.py',131),
  ('print_statement -> TAB STRING SEMICOLON','print_statement',3,'p_print_statement','interpreter.py',135),
  ('assignment_statement -> ID ORO expression SEMICOLON','assignment_statement',4,'p_assignment_statement','interpreter.py',139),
  ('function_definition -> BUILD ID COLON statements','function_definition',4,'p_function_definition','interpreter.py',143),
  ('expression -> NUMBER','expression',1,'p_expression','interpreter.py',147),
  ('expression -> STRING','expression',1,'p_expression','interpreter.py',148),
  ('expression -> ID','expression',1,'p_expression','interpreter.py',149),
]
