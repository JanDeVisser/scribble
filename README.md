# scribble

## Grammar

```ebnf
program                 ::= module modules
                          ;
modules                 ::= module modules
                          | 
                          ;
module                  ::= module_level_statements
                          ;
module_level_statements ::= module_level_statement ';' module_level_statements
                          | 
                          ;
module_level_statement  ::= variable_declaration
                          | type_declaration
                          | native_declaration
                          | function
                          ;
variable_declaration    ::= var_decl_keyword identifier var_type var_initializer
                          ;
var_decl_keyword        ::= 'const' | 'var'
                          ;
var_type                ::= ':' typespec
                          | 
                          ;
typespec                ::= identifier
                          ;
variable_initializer    ::= '=' expression
                          | 
                          ;
type_declaration        ::= struct_declaration
                          ;
struct_declaration      ::= 'struct' '{' struct_component struct_components '}'
                          ;
struct_components       ::= struct_component struct_components
                          | 
                          ;
struct_component        ::= identifier ':' typespec ';'
                          ;
native_declaration      ::= function_declaration '->' quoted_string
                          ;
function_declaration    ::= 'func' identifier '(' parameters ')' var_type
                          ;
parameters              ::= non_empty_parameters
                          | 
                          ;
non_empty_parameters    ::= parameter 
                          | parameter ',' non_empty_parameters
                          ;
parameter               ::= identifier ':' typespec
                          ;
function                ::= function_declaration function_implementation
                          ;
function_implementation ::= statement
                          ;
statement               ::= variable_declaration ';'
                          | assignment ';'
                          | if_statement
                          | while_statement
                          | loop_statement
                          | for_statement
                          | return_statement ';'
                          | block
                          | break_statement ';'
                          | continue_statement ';'
                          ;
assignment              ::= identifier assignment_operator expression
                          ;                          
assignment_operator     ::= '=' | '+=' | '-=' | '*=' | '/=' | '%=' 
                          | '|=' | '&=' | '<<=' | '>>='
                          ;
if_statement            ::= 'if' expression statement elif_clause
                          ;
elif_clause             ::= 'elif' expression statement elif_clause
                          | 'else' statement
                          |
                          ;
while_statement         ::= label 'while' expression statement
                          ;       
label                   ::= identifier ':'
                          |
                          ;                                                                                                 
loop_statement          ::= label 'loop' statement
                          ;                                                                              
for_statement           ::= label 'for' identifier 'in' expression statement
                          ;       
return_statement        ::= 'return' optional_expression
                          ;                     
optional_expression     ::= expression
                          |
                          ;                   
block                   ::= '{' statements '}'
                          ;                                     
statements              ::= statement statements
                          | 
                          ;
break_statement         ::= 'break' identifier;
                          ;                         
continue_statement      ::= 'continue' identifier;
                          ;        
expression              ::= comparison_predicate comparison_operator comparison_predicate
                          | comparison_predicate
                          ;
comparison_predicate    ::= equality_predicate equality_operator equality_predicate
                          | equality_predicate
                          ;
equality_predicate      ::= term additive_operand term
                          | term
                          ;                          
term                    ::= factor multiplicative_operator factor
                          | factor
                          ;        
factor                  ::= '(' expression ')'
                          | primary
                          ;
primary                 ::= function_call
                          | integer
                          | identifier
                          | quoted_string
                          ;
function_call           ::= identifier '(' arguments ')'
                          ;
arguments               ::= non_empty_arguments
                          | 
                          ;
non_empty_arguments     ::= expression 
                          | expression ',' non_empty_arguments
                          ;
additive_operator       ::= '+' | '-'
                          ;                          
multiplicative_operator ::= '*' | '/' | '%'
                          ;                          
equality_operator       ::= '=' | '!='
                          ;                          
comparison_operator     ::= '<' | '<=' | '>' | '>='
                          ;                          
```
