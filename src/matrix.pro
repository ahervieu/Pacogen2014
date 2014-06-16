:-use_module(library(objects)). 
:-use_module(library(lists)).
:-use_module(library(clpfd)).
:-load_files(library(obj_decl),[when(compile_time), if(changed)]).

/* matrice(M,X,Y) : Matrice M, X : nombre de colonnes, et y nombre de lignes */
/* Crée une liste de colonne */

matrice([],0,_).
matrice([L|M],X,Y) :- X >0 , X1 is X -1 ,length(L,Y), matrice(M, X1, Y) .  


writeMatrixFile(File,M1) :- mytranspose(M1,M2),open(File, append, Stream),nl(Stream),writeMatrice(M2,Stream),nl(Stream),close(Stream).



printMatrixScreen(M1):-
        mytranspose(M1,M2),
        printMatrixScreen0(M2).

printMatrixScreen0([A|B]) :-
       writelineScr(A),
       write('\n'),
       printMatrixScreen0(B).
printMatrixScreen0([]).

writelineScr([A|B]):- 
        write(A), write(','), writelineScr(B).
writelineScr([]).

writeMatrice([A|B],S) :- 
        writeligne(A,S), 
        write(S,'\n'), 
        writeMatrice(B,S).
writeMatrice([],_).

writeligne([A|[]],S):- 
        write(S,A),!.
writeligne([A|B],S):- 
        write(S,A), 
        write(S,','), 
        writeligne(B,S).
writeligne([],_).
/*
Transpose(M1,M2)
*/

mytranspose(M1,M2):- nbline(M1,_), creeMat(M1,M2,1).
creeMat(M1,[],K) :- nbline(M1,I), K is I +1.
creeMat(M1,R,I):-nbline(M1,K), I =< K, line(M1,I,Li), I2 is I +1, R = [Li|M12], creeMat(M1,M12,I2).



/* elementdeL(L,I,E) :  L : liste, I : rang de l'élément, E : élement */
/*
nth(?N, ?List, ?Element)
*/
elementdeL([A|_],1,A).
elementdeL([_|B],I,C):- I >1 , I1 is I-1 , elementdeL(B,I1 ,C).

/* Obtenir Colonne*/
/* Colonne(M,I,C) */
/* M : matrice définie précedement, I : l'indice de colonne, C : la colonne résultante */ 

colonne(M,I,C):- elementdeL(M,I,C).


nbline([A|_],L):- length(A,L).
/* Obtenir Ligne */
/* ligne(M,I,L) : M : matrice définie précédement, I : indice de ligne, L : la ligne resultante */

line([],_,[]).
line([A|R],I,[B|T]) :-nth1(I,A,B) , line(R,I,T).

/* variable(M,V) : obtention de la liste des variable de M */
variable([],[]).
variable([A|R],L):- variable(R,L2),append(A,L2,L). 

/* -*- Fin de la définition de la structure de donnée -*- */
