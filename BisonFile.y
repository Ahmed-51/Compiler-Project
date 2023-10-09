/* C Declarations */

%{
	#include<stdio.h>
	#include<stdlib.h>
	#include <string.h>
	#include <math.h>
    int mark1=0;
	int pop=1;
	int switchdone = 0;
	int switchvar;
    int save=0;
	int ifval[1000];
	int ifptr = -1;
	int ifdone[1000];
    int okk=0;
    int ptr = 0;
    int value[1000];
    char varlist[1000][1000];
    char keep[1000][1000];
	int v1[100];
	int mark=0;
    ///if already declared  return 1 else return 0
    int isdeclared(char str[]){
        int i;
        for(i = 0; i < ptr; i++){
            if(strcmp(varlist[i],str) == 0) return 1;
        }
        return 0;
    }
    /// if already declared return 0 or add new value and return 1;
    int addnewval(char str[],int val){
        if(isdeclared(str) == 1) return 0;
        strcpy(varlist[ptr],str);
        value[ptr] = val;
        ptr++;
        return 1;
    }

    ///get the value of corresponding string
    int getval(char str[]){
        int indx = -1;
        int i;
        for(i = 0; i < ptr; i++){
            if(strcmp(varlist[i],str) == 0) {
                indx = i;
                break;
            }
        }
        return value[indx];
    }
    int setval(char str[], int val){
    	int indx = -1;
        int i;
        for(i = 0; i < ptr; i++){
            if(strcmp(varlist[i],str) == 0) {
                indx = i;
                break;
            }
        }
        value[indx] = val;

    }


%}

%union {
  char text[1000];
  int val;
}


%token <text>  ID
%token <val>  NUM
%token <text> STR

%type <val> expression
%token INT DOUBLE CHAR SINE GCD POW LCM COS TAN asin1 acos1 SORT PRIME Print MAIN TO BREAK CMP REV FACT CUM CMP1 MIN fbs fbe sbs sbe tbs tbe CM SM PRINTVAR MAX PRINTSTR PRINTLN WHILE SMGT LT EQ NEQ GTE LTE ASSIGN PLUS MINUS MUL DIV REM IF ELSE ELSEIF FOR INC DEC SWITCH CASE DEFAULT COL FUNCTION

%left LT GT LTE GTE
%left PLUS MINUS
%left MUL DIV


%nonassoc IFX
%nonassoc ELSE
%left SH


%%

starthere 	: function program function
			;

program		: INT MAIN fbs fbe sbs statement sbe { printf("\nCompilation Successful\n"); }
			;
statement	: /* empty */
			| statement declaration
			| statement print
			| statement expression 
			| statement ifelse
			| statement assign
			| statement forloop
			| statement switch
			| statement extend  
			| statement sob
			;

statement1	: /* empty */
			| statement1 temp
			;

lost	: /* empty */
			| lost lol
			;
/*--------declarationsbegin--------*/


declaration : type variables SM {}
			;
type		: INT | DOUBLE | CHAR {}
			;
variables	: variable CM variables {}
			| variable {}
			;
variable   	: ID 	
					{
						int x = addnewval($1,0);
						if(!x) {
							printf("Compilation Error:Variable %s is already declared\n",$1);
							exit(-1);
						}

					}
			| ID ASSIGN expression 	
					{
						int x = addnewval($1,$3);
						if(!x) {
							printf("Compilation Error: Variable %s is already declared\n",$1);
							exit(-1);
							}
					}

			;

/*-------declaration end----------*/

/*------variable assignsbegin-----*/
sob: 	 MAX fbs expression fbe SM
		{
		  printf("%d\n",$3);
		}
		| MIN fbs expression fbe SM
		{
		  printf("%d\n",$3);
		}
		| fbs expression fbe SM
		{
		 
		}
		| FACT expression SM
		{
		 int mul=1,i=0;
		 for(i=1;i<=$2;i++)
		 mul*=i;
		  printf("%d\n",mul);
		}
		| SINE expression SM
		{
		double pi=acos(-1);
		  double a=$2*pi;
		  a/=180;
		 pi=sin(a);
		  printf("%.6f\n",pi);
		}
		| COS expression SM
		{
		double pi=acos(-1);
		  double a=$2*pi;
		  a/=180;
		 pi=cos(a);
		  printf("%.6f\n",pi);
		}
		| CUM expression SM
		{
		 int mul=0,i=0;
		 for(i=1;i<=$2;i++)
		 mul+=i;
		  printf("%d\n",mul);
		}
		| PRIME expression SM
		{
		
		int i,j;  
		for(i=2; i<=$2; i++)
		{
		int x=0;
        for(j=2; j*j<=i; j++)
        {
            if(i%j==0)
            {
               x=1;
			   break;
            }
        }
		if(!x)printf("%d ",i);
		}
		}
		| REV fbs STR fbe SM
		{
		 int l = strlen($3);
		int i;
		for(i = l-2;  i >0; i--) printf("%c",$3[i]);
		
		}
		| SORT fbs STR fbe SM
		{
		int l = strlen($3);
		int i,j;
		for(i=1; i<l-1; i++)
		{
        for(j=i+1; j<l-1; j++)
        {
            if($3[i] > $3[j])
            {
                char temp = $3[i];
                $3[i] = $3[j];
                $3[j] = temp;
            }
        }
		}
		for(i = 1;  i < l-1; i++) printf("%c",$3[i]);
		}
		;
assign : ID ASSIGN expression SM  
					{
						if(!isdeclared($1)) {
							printf("Compilation Error: Variable %s is not declared\n",$1);
							exit(-1);
						}
						else{
							setval($1,$3);
						}
				    }

/*------variable assign end-------*/


/*--------printingsbegin----------*/

print		: PRINTVAR fbs ID fbe SM 	
					{
						if(!isdeclared($3)){
							printf("Compilation Error: Variable %s is not declared\n",$3);
							exit(-1);
						}
						else{
							int v = getval($3);
							printf("%d",v);
						}
					}
			| PRINTSTR fbs STR fbe SM 
					{
						int l = strlen($3);
						int i;
						for(i = 1;  i < l-1; i++) printf("%c",$3[i]);
					}
			| PRINTLN fbs fbe SM 	
					{
						printf("\n");
					}
			;
lol		: PRINTVAR fbs ID fbe SM 	
					{
						pop++;
						if(!isdeclared($3)){
							printf("Compilation Error: Variable %s is not declared\n",$3);
							exit(-1);
						}
						else{
							mark=1;
				      		v1[pop] = getval($3);

						}
					}
			| PRINTSTR fbs STR fbe SM 
					{
					pop++;
					mark=2;
					strcpy(keep[pop],$3);	
					}
			| PRINTLN fbs fbe SM 	
					{
						pop++;
						mark=3;
					}
			;
			
temp		: PRINTVAR fbs ID fbe SM 	
					{
						if(!isdeclared($3)){
							printf("Compilation Error: Variable %s is not declared\n",$3);
							exit(-1);
						}
						else{
						    int k=0;
							for(k = 1; k <= save; k += okk){
    							int v = getval($3);
								printf("%d",v);
							}
						}
					}
			| PRINTSTR fbs STR fbe SM 
					{
						int l = strlen($3);
						int k = 0,i=0;
						for(k = 1; k <= save; k += okk){
							for(i = 1;  i < l-1; i++) { printf("%c",$3[i]);
						}
						}
					}
			| PRINTLN fbs fbe SM 	
					{
						int k = 0;
						for(k = 1; k <= save; k += okk){
						printf("\n");
						}
					}
			;


/*--------printing end------------*/

/*--------expressionsbegin--------*/

expression : NUM {$$ = $1;}
			| ID 	
					{
						if(!isdeclared($1)) {
							printf("Compilation Error: Variable %s is not declared\n",$1);
							exit(-1);
						}
						else{
							$$ = getval($1);
						}
				 	}
			| expression PLUS expression 
					{$$ = $1 + $3;}
			| expression MINUS expression 
					{$$ = $1 - $3;}
			| expression MUL expression 
					{$$ = $1 * $3;}
			| expression CMP expression 
					{
					if($1>$3) $$=$1;
					else $$=$3;
					}
			| expression CUM 
					{
					 $$=$1;
					}
			| expression CMP1 expression 
					{
					if($3>$1) $$=$1;
					else $$=$3;
					}
			| expression GCD expression 
					{
					int n1=$1;
					int n2=$3;
					
					while(n1!=n2)
					{
					if(n1 > n2)
					n1 -= n2;
					else
					n2 -= n1;
					}printf("%d\n",n1);
					}
			| expression LCM expression 
					{
					int n1=$1;
					int n2=$3;
					int a=n1*n2;
					while(n1!=n2)
					{
					if(n1 > n2)
					n1 -= n2;
					else
					n2 -= n1;
					}
					n1=a/n1;
					printf("%d\n",n1);
					}
			| expression POW expression 
					{
					int n1=$1;
					int n2=$3;
					int ans=1;
					while(n2)
					{
					ans*=n1;
					n2--;
					}
					printf("%d\n",ans);
					}
			| expression DIV expression 
					{
						if($3) {
 							$$ = $1 / $3;
							}
				  		else {
							$$ = 0;
							printf("\nRuntime Error: division by zero\t");
							exit(-1);
				  		} 
					}
			| expression LT expression	
					{ $$ = $1 < $3; }
			| expression GT expression	
					{ $$ = $1 > $3; }
			| expression LTE expression	
					{ $$ = $1 <= $3; }
			| expression GTE expression	
					{ $$ = $1 >= $3; }
			| fbs expression fbe
					{$$ = $2;}
			;


/*--------expressionsbegin--------*/


/*---------ifelsesbegin----------*/

ifelse 	: IF fbs ifexp fbe sbs lost sbe elseif %prec IFX
					{

						ifdone[ifptr] = 0;
						ifptr--;
						if(mark1){
						
						if(mark==1)
							{
							printf("%d",v1[0]);
							}
							else if(mark==2)
							{
							int i;
							int l=strlen(keep[0]);
							for(i = 1;  i < l-1; i++) printf("%c",keep[0][i]);
							}
							else
							{
							printf("\n");
							}
							}
							
					}
		;
ifexp	: expression 
					{			
                        pop=-1;
					    mark1=0;
						ifptr++;
						ifdone[ifptr] = 0;
						if($1){
							mark1=1;
							ifdone[ifptr] = 1;
							
						}	
					}						

		;
elseif 	: /* empty */
		| elseif ELSEIF fbs expression fbe sbs lost sbe 
					{
						if($4 && ifdone[ifptr] == 0){
							ifdone[ifptr] = 1;
							
							if(mark==1)
							{
							printf("%d",v1[1]);
							}
							else if(mark==2)
							{
							int i;
							int l=strlen(keep[1]);
							for(i = 1;  i < l-1; i++) printf("%c",keep[1][i]);
							}
							else
							{
							printf("\n");
							}
							}
						}
		| elseif ELSE sbs lost sbe
					{
						if(ifdone[ifptr] == 0){
							ifdone[ifptr] = 1;
							if(mark==1)
							{
							printf("%d",v1[2]);
							}
							else if(mark==2)
							{
							int i;
							int l=strlen(keep[2]);
							for(i = 1;  i < l-1; i++) printf("%c",keep[2][i]);
							}
							else
							{
							printf("\n");
							}
							}
						}

		;

/*---------ifelse end------------*/


/*------foor loopsbegin----------*/


forloop	: FOR fbs expression TO expression INC expression  	
					{
						int st = $3;
						int ed = $5;
						int dif = $7;
						int cnt = 0;
						int k = 0;
						okk=dif;
						for(k = st; k <= ed; k += dif){
							cnt++;
						}	
						save=cnt;
						
					}
extend 	: fbe sbs statement1 sbe				

/*------foor loop end------------*/


/*------switch casesbegin--------*/

switch	: SWITCH fbs expswitch fbe sbs switchinside sbe 
		;

expswitch 	:  expression 
					{
						switchdone = 0;
						switchvar = $1;
					}
			;


switchinside	: /* empty */
				| switchinside expression COL sbs statement sbe 
					{
						if($2 == switchvar){
							printf("Executed %d\n",$2);
							switchdone = 1;
						}					
					}
				| switchinside DEFAULT COL sbs statement sbe 
					{
						if(switchdone == 0){
							switchdone = 1;
							printf("Default Block executed\n");
						}
					}
				;


/*------switch case end----------*/

/*------functionsbegin-----------*/

function 	: /* empty */
			| function func
			;

func 	: type FUNCTION fbs fparameter fbe sbs statement sbe
					{
						printf("Function Declared\n");
					}
		;
fparameter 	: /* empty */
			| type ID fsparameter
			;
fsparameter : /* empty */
			| fsparameter CM type ID
			;


/*-------function end------------*/
%%


int yyerror(char *s){
	printf( "%s\n", s);
}