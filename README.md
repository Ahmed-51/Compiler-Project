# Compiler-Project

FLEX (fast lexical analyzer generator) is a computer program for generating lexical analyzers. It is used together with GNU Bison parser generator. BISON is a general-purpose parser generator that converts a grammar description for an LALR(1) context-free grammar into a C program to parse that grammer. It interfaces with scanner generated by Flex and Scanner called as a subroutine when parser needs the next token. It automatically write a parser program for a grammar written in BNF.
Bison produces parser from the input file provided by the user. The function yylex() is automatically generated by the flex when it is provided with a .l file and this yylex() function is expected by parser to call to retrieve tokens from current/this token stream. 
The function yylex() is the main flex function that runs the Rule Section and extension (.l) is the extension used to save the programs.


### Procedure
1. The code is divided into two parts: flex file (.l) and bison file (.y)
2. Input expression checks the lex (.y) file and if the expression satisfies the rule then it checks the CFG into the bison file.
3. It’s a bottom up parser and the parser constructs the parse tree. If the leaves node matches with the rules and if the CFG matches then it gradually goes to the root.


### Commands to run the program in terminal
1. flex FlexFile.l
2. bison -d BisonFile.y
3. gcc lex.yy.c  FlexFile.tab.c  -o  app
4. app

