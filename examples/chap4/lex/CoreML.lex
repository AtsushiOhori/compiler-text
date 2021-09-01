type lexresult = Token.token
val eof = fn () => Token.EOF
fun atoi s = valOf (Int.fromString s)
%%
%structure CoreMLLex
alpha   = [A-Za-z];
digit   = [0-9];
id      = {alpha}({alpha}|{digit})*;
num     = {digit}+;
frac    = "."{num};
exp     = [eE](~?){num};
real    = (~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
ws      = "\ " | "\t" | "\r\n" | "\n" | "\r";
%%
\"[^"]*\" => (Token.STRING 
              (String.substring
                 (yytext,1,String.size yytext - 2)));
"_"       => (Token.UNDERBAR);
{id}      => (Token.ID yytext);
{real}    => (Token.REAL yytext);
{ws}      => (lex());
.         => (Token.SPECIAL yytext);
