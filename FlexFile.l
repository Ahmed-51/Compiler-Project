char [a-zA-Z]
digit [0-9]

%{
	#include "BisonFile.tab.h"
	#include <string.h>
%}



%%
"#".*"#" {printf("A Single line comment\n");}
"*".*"*" {printf("A Header File is Included\n");}
[ \n] {}
";" {return SM;}
"," {return CM;}
"/*"([^*]|\*+[^*/])*\*+"/" {printf("A Multiline comment\n"); }
{digit}+ 	{yylval.val = atoi(yytext);return NUM;}
"Main" { return MAIN;}
"PrintVar" { return PRINTVAR;}
"PrintStr" {return  PRINTSTR;}
"PrintLine" {return PRINTLN;}
"Func_"({char}|{digit})+ {return FUNCTION;}
"Var_"({char}|{digit})+ { strcpy(yylval.text,yytext);return ID;}
"Int" {  return INT;}
"Double" { return DOUBLE;}
"Char" { return CHAR;}
"(" { return fbs;}
")" { return fbe;}
"{" { return sbs;}
"}" { return sbe;}
"Shoman" {return ASSIGN;}
"Jog" {return PLUS;}
"Biyog" {return MINUS;}
"Gun" {return MUL;}
"Vag" {return DIV;}
"Lt" {return LT;}
"Gt" {return GT;}
"Lte" {return LTE;}
"Gte" {return GTE;}
"Max"  {return MAX;}
"Min"  {return MIN;}
"Cmp" {return CMP;}
"Cmp1" {return CMP1;}
"Fact" {return FACT;}
"CumAdd" {return CUM;}
"Rev" {return REV;}
"Sort" {return SORT;}
"Prime" {return PRIME;}
"Sin" {return SINE;}
"Cos" {return COS;}
"Gcd" {return GCD;}
"Lcm" {return LCM;}
"Power" {return POW;}
"ElseIf" {return ELSEIF;}
"If" { return IF;}
"Else" {return ELSE;}
"For" {return FOR;}
"IncrementBy" {return INC;}
"To" {return TO;}
"Switch" {return SWITCH;}
"*" {return DEFAULT;}
":" {return COL;}
. {}
"\""(.)*"\"" {strcpy(yylval.text,yytext);return STR;}




%%


int yywrap()
{
	return 1;
}


int main(){
	yyin = freopen("InputFile.txt","r",stdin);
	freopen("OutputFile.txt","w",stdout);
	yyparse();
}