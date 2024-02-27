%{
#include <stdio.h>
void yyerror (const char *msg){
return; }    
%}
%token tMAIL tENDMAIL tSCHEDULE tENDSCHEDULE tSEND tSET tTO tFROM tAT tCOMMA tCOLON tLPR tRPR tLBR tRBR tIDENT tSTRING tADDRESS tDATE tTIME
%start program

%%

program: /* empty */ 
       | components 
       ;

components: mail_block components
          | set_statement components
          | /* empty */
          ;

mail_block: tMAIL tFROM tADDRESS tCOLON statement_list tENDMAIL
          | tMAIL tFROM tADDRESS tCOLON tENDMAIL
          ;

statement_list: statement
              | statement_list statement
              ;

set_statement: tSET tIDENT tLPR tSTRING tRPR
            ;

statement: set_statement
         | send_statement
         | schedule_statement
         ;

send_statement: tSEND tLBR tSTRING tRBR tTO recipient_list 
              | tSEND tLBR tIDENT tRBR tTO recipient_list
              ;

schedule_statement: tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON send_statement_list tENDSCHEDULE
                ;

send_statement_list: send_statement
                  | send_statement_list send_statement
                  ;

recipient_list: tLBR recipient_objects tRBR
             ;

recipient_objects: recipient_object
                | recipient_objects tCOMMA recipient_object
                ;

recipient_object: tLPR tSTRING tCOMMA tADDRESS tRPR
                | tLPR tADDRESS tRPR
                | tLPR tIDENT tCOMMA tADDRESS tRPR
                ;

%%

int main()
{
    if(yyparse()){
        // parse error
        printf("ERROR\n");
        return 1;
    }
    else{
        // successful parsing
        printf("OK\n");
        return 0;
    }

}