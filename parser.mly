%{
        (* --- préambule: ici du code Caml --- *)

open Simplex
open InstanceBuilder

        %}
/* description des lexèmes */

%token <string> VAR
%token <float> VAL
%token PLUS, MINUS
%token LEQ, GEQ, EQ
%token MIN, MAX
%token ST, BDS, VARS
%token EOL, EOF            /* retour à la ligne */

%start main             /* "start" signale le point d'entrée: c'est ici main */
%type <Simplex.simplex> main     /* on _doit_ donner le type du point d'entrée */

%%
main:
  |EOL main                             { $2 }
  |objective EOL objectiveFunction EOL ST EOL constraints BDS EOL bounds VARS EOL variables EOF { new Simplex.simplex $1 $3 $7 $10 $13 }
  |objective EOL objectiveFunction EOL ST EOL constraints BDS EOL VARS EOL variables EOF { new Simplex.simplex $1 $3 $7 [] $12 }
  ;

  objective:
  |MAX                                  { true }
  |MIN                                  { false }
  ;

  objectiveFunction:
  |expression                           { $1 }
  ;

  expression:
  |PLUS atomicExpression                { [$2] }
  |MINUS atomicExpression               { let (var, coeff) = $2 in [(var, -.coeff)] }
  |expression PLUS atomicExpression     {$3 :: $1 }
  |expression MINUS atomicExpression    { let (var, coeff) = $3 in (var, -.coeff) :: $1 }
  |atomicExpression                     { [$1] }
;
  
  atomicExpression:
  |VAL VAR                              { ($2, $1) }
  |VAR                                  { ($1, 1.) }
;

  constraints:
  |singleConstraint EOL constraints     { $1 @ $3 }
  |singleConstraint EOL                 { $1 }
;

  singleConstraint: /* constraint is a reserved word */
  |expression LEQ VAL                   { [($3, $1)] }
  |expression GEQ VAL                   { [(-.$3, minusExpression $1)] }
  |expression EQ  VAL                   { [($3, $1); (-.$3, minusExpression $1)] }
;

  bounds:
  |bound EOL bounds                     { $1 @ $3 }
  |bound EOL                            { $1 }
;
  bound:
  |VAR GEQ VAL                          { [(-.$3, [($1, -.1.)])] }
  |VAR LEQ VAL                          { [($3, [($1, 1.)])] }
  |VAL LEQ VAR LEQ VAL                  { [($1, [($3, 1.)]); (-.$5, [($3, -.1.)])] }
  |VAL GEQ VAR GEQ VAL                  { [(-.$1, [($3, -.1.)]); ($5, [($3, 1.)])] }
  ;

  variables:
  |VAR EOL variables                    { VariableSet.add $1 $3 }
  |VAR EOL                              { VariableSet.singleton $1 }
  ;
