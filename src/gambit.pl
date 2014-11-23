%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% gambit: cálculo lambda <prolog>                       %
% Universidad de Costa Rica, 2014                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%
% E V A L U A C I O N E S % evaluación de una expresión lambda
%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Reduce una expresión
    evaluate(E,NE) :-       reduce(E,TempE),        % 
                            print(TempE),nl,nl,
                            evaluate(TempE,NE).
    % Caso por defecto(no se puede reducir la expresión)
    evaluate(E,E).

%%%%%%%%%%%%%%%%%%%%%%%%%
% R E D U C C I O N E S % sustitución de una expresión lambda
%%%%%%%%%%%%%%%%%%%%%%%%%
    % Aplica substitución de argumentos
    % Reducción beta(forma normal)
    reduce(comb(lamb(X,Body),Arg),R) :- 
                                        write('Rator = '),print(lamb(X,Body)),nl,
                                        write('Rand = '),print(Arg),nl,nl,
                                        subst(Body,X,Arg,R).

    % Reducir una expresión de una función predefinida(reducción )
    reduce(comb(con(C),con(Arg)),R) :- compute(C,Arg,R). 

    % Reducir expresiones lambda tipo opeRAND y opeRATOR
    reduce(comb(Rator,Rand),comb(NewRator,Rand)) :- reduce(Rator,NewRator).
    reduce(comb(Rator,Rand),comb(Rator,NewRand)) :- reduce(Rand,NewRand).

    % Reducir el cuerpo de una expresión lambda
    reduce(lamb(X,Body),lamb(X,NewBody)) :- reduce(Body,NewBody).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S U B S T I T U C I O N E S % reemplazo de una variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Una variable cualquiera
    subst(var(V),V,E1,E1).
    % Una variable X diferente de V
    subst(var(X),V,E1,var(X)).
    % Una constante cualquiera
    subst(con(C),V,E1,con(C)).
    %  Un operando asociado a un operador
    subst(comb(Rator,Rand),V,E1,comb(NewRator,NewRand)) :- 
                                             subst(Rator,V,E1,NewRator),
                                             subst(Rand,V,E1,NewRand).
    % Una expresión lambda de una variable asociada
    subst(lamb(V,E),V,E1,lamb(V,E)).
    % Una expresión lambda de una variable libre
    subst(lamb(X,E),V,E1,lamb(Z,NewE)) :-       freevars(E1,F1),
                                                (member(X,F1),
                                                freevars(E,F), 
                                                union(F,[V],F2), union(F1,F2,FV),
                                                variant(X,FV,Z),
                                                subst(E,X,var(Z),TempE),
                                                subst(TempE,V,E1,NewE);
                                                subst(E,V,E1,NewE), Z=X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% V A R I A B L E S - L I B R E S % Variales globales, sin declaración
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Una variable libre X cualquiera
    freevars(var(X),[X]).
    % Una constante libre C cualquiera > nula
    freevars(con(C),[]).
    % VL de una aplicación(Operando y Operador)
    freevars(comb(Rator,Rand),FV) :- freevars(Rator,RatorFV),freevars(Rand,RandFV),
                                     union(RatorFV,RandFV,FV).
    % Una variable de una expresión lambda
    freevars(lamb(X,E),FV) :-        freevars(E,F),delete(X,F,FV).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
% C O M P U T A C I O N E S % Operadores aplicados a una función lambda
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % sucesor
    compute(succ,N,con(R)) :- R is N+1.
    % predecesor
    compute(pred,N,con(R)) :- N>0, R is N-1.
    compute(pred,0,con(0)).
    % raiz cuadrada
    compute(sqr,N,con(R)) :- R is N*N.
    % suma 
    compute(add,N,con(add(N))).
    compute(add(M),N,con(R)) :- R is M+N.
    % resta
    compute(sub,N,con(sub(N))).
    compute(sub(M),N,con(R)) :- R is M-N.
    % producto
    compute(mul,N,con(mul(N))).
    compute(mul(M),N,con(R)) :- R is M*N.
    % división
    compute(div,N,con(div(N))).
    compute(div(M),N,con(R)) :- R is M//N.

%%%%%%%%%%%%%%%%%%%%%%%%%
% V A R I A C I O N E S %
%%%%%%%%%%%%%%%%%%%%%%%%%
    % marcar una variable
    prime(X,PrimeX) :-      name(X,L), concat(L,[39],NewL), name(PrimeX,NewL).
    % obtiene una variable diferente a todas las variables de la lista L
    variant(X,L,NewX) :-    member(X,L),prime(X,PrimeX),variant(PrimeX,L,NewX).
    % caso por defecto
    variant(X,L,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% O T R A S - F U N C I O N E S %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % unión de listas
    union([],S,S).
    union([H|T],S,U) :- member(H,S),union(T,S,U).
    union([H|T],S,[H|U]) :- union(T,S,U).

    % eliminar un elemento de una lista
    delete(X,[],[]).
    delete(H,[H|T],R) :- delete(H,T,R).
    delete(X,[H|T],[H|R]) :- delete(X,T,R).

    % concatenar dos listas
    concat([],L,L).
    concat([H|T],L,[H|M]) :- concat(T,L,M).

    % elemento miembro de una lista
    member(H,[H|T]).
    member(X,[H|T]) :- member(X,T).

%%%%%%%%%%%%%%%
% O U T P U T %
%%%%%%%%%%%%%%%
    print(var(X)) :- write(X).      print(con(C)) :- write(C).
    print(lamb(X,E)) :- write('(L '),write(X),tab(1),print(E),write(')').
    print(comb(Rator,Rand)) :- write('('),print(Rator),tab(1),print(Rand),write(')').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% H I L O - P R I N C I P A L %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    go :- 
        nl,write('Calculo λ'),nl,nl,
        write('Nombre de archivo:  '), nl, getfilename(FileName),nl,
        see(FileName), scan(Tokens), nl, write('Scan: [OK]'), nl, !,
        write(Tokens), nl, nl,
        seen, program(expr(E),Tokens,[eop]),write('Parse: [OK]'),nl,!,
        write(expr(E)), nl, nl,
        evaluate(E,Result),nl,write('Resultado =  '),print(Result),nl.

%%%%%%%%%%%%%%%
% P A R S E R %
%%%%%%%%%%%%%%%
    program(expr(E)) --> expr(E).

    % Análisis sintáctico de una expresión lambda(forma reducida)
    expr(lamb(X,E)) --> [lparen],['L'],[var(X)],expr(E1),readlamb(E1,E).
    % Sentencias lambda(forma reducida)
    readlamb(E,E) --> [rparen].
    readlamb(var(Y),lamb(Y,E)) --> expr(E1),readlamb(E1,E).
    % Análisis sintáctico de una expresión lamba(aplicación)
    expr(E) --> [lparen],expr(E1),expr(E2),readcomb(E1,E2,E).
    % Sentencias tipo aplicación lambda
    readcomb(E1,E2,comb(E1,E2)) --> [rparen].
    readcomb(E1,E2,E) --> expr(E3), readcomb(comb(E1,E2),E3,E).

    % Expresiones de operaciones definidas
    expr(var(X)) --> [var(X)].          % variable
    expr(con(X)) --> [num(X)].          % constante
    expr(con(add)) --> [add].           % suma
    expr(con(sub)) --> [sub].           % resta
    expr(con(mul)) --> [mul].           % producto
    expr(con(div)) --> [div].           % división
    expr(con(succ)) --> [succ].         % sucesor
    expr(con(pred)) --> [pred].         % predecesor
    expr(con(sqr)) --> [sqr].           % raíz cuadrada
    expr(con(true)) --> [true].         % verdadero
    expr(con(false)) --> [false].       % falso


%%%%%%%%%%%%%%%%%%%
% A R C H I V O S %
%%%%%%%%%%%%%%%%%%%
    % Obtener nombre de archivo
    getfilename(F) :- get0(C),readfilename(C,Cs),name(F,Cs).
    readfilename(C,[C|Cs]) :- filechar(C),get0(D),readfilename(D,Cs).
    readfilename(C,[]).

%%%%%%%%%%%%%%%%%
% S C A N N E R %
%%%%%%%%%%%%%%%%%
    % análisis léxico del programa
    scan([T|Lt]) :-  tab(4), getch(C), gettoken(C, T, C1), readprog(T, C1, Lt).
    
    % obtener caracter
    getch(C) :- get0(C),(endline(C),nl,tab(4) ; endfile(C), nl; put(C)).
    
    % leer un programa
    readprog(eop, C, []).        % fin de archivo
    
    % leer un programa, palabra por palabra
    readprog(T, C, [U|Lt]) :-  gettoken(C, U, D), readprog(U, D, Lt).
    
    % obtener palabra(variable o palabra reservada)
    gettoken(C, eop, 0) :-     endfile(C).  % fin de archivo
    gettoken(C, T, D)  :-      single(C,T), getch(D).    % parentesis
    gettoken(C, T, E)  :-      letter(C), getch(D), readvar(D, Lc, E),    % letra(variable)
                               name(V, [C|Lc]), (resword(V),T=V ; T=var(V)).
    % variable válida
    readvar(C, [C|Lc], E) :- idchar(C), getch(D),readvar(D,Lc,E).
    % variable inválida
    readvar(C, [], C).

    % obtener palabra(número)
    gettoken(C, num(N), E) :-   digit(C), getch(D), readvar(D, Lc, E),
                                name(N, [C|Lc]).
    % Digito válido
    readnum(C, [C|Lc], E) :-     digit(C), getch(D), readnum(D, Lc, E).
    % Digito inválido
    readnum(C, [], C).

    % obtener palabra(otras, inválidas)
    gettoken(C, T, E)   :- whitespace(C), getch(D), gettoken(D,T,E).
    gettoken(C, T, E)   :- write('Caracter desconocido> '), put(C), nl, abort.

    % Palabras reservadas
    resword('L').             % lambda
    resword(succ).            % sucesor
    resword(pred).            % predecesor
    resword(add).             % suma
    resword(sub).             % resta
    resword(mul).             % producto
    resword(div).             % división
    resword(sqr).             % raíz cuadrada
    resword(true).            % verdadero
    resword(false).           % falso

    % Otras operaciones
    single(40,lparen).            % parentesis der
    single(41,rparen).            % parentesis izq
    lower(C) :- 97=<C,C=<122.     % a - z
    upper(C) :- 65=<C,C=<90.      % A - Z
    digit(C) :- 48=<C,C=<57.      % 0 - 9
    space(32).                    % ' '
    tabch(9).                     % \t
    period(46).                   % .
    slash(47).                    % /
    endline(10).                  % \n
    endfile(26).                  % eof
    endfile(-1).                  % eof
    prime(39).                    % apostrofe

    % espacio en blanco
    whitespace(C) :- space(C) ; tabch(C) ; endline(C).
    % letra
    letter(C) :- lower(C) ; upper(C). 
    % identificador(var)
    idchar(C) :- letter(C) ; digit(C) ; prime(C).
    % archivo
    filechar(C) :- letter(C) ; digit(C) ; period(C) ; slash(C).