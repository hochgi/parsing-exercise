grammar SBTree;

sbtree : VALUE
       | VALUE '(' sbtree (',' sbtree)* ')'
       ;

VALUE : [0-9A-Za-z]* ;