:- use_module(library(objects)). 
:-use_module(library(lists)).
:- load_files(library(obj_decl),[when(compile_time), if(changed)]).


/*
Pair class
v1 : value of the first term of the  pair
C1 : value of the column  of the first term

v2 : value of the second term of the  pair
C2 : column number of the second term
*/

:-class pair = [public v1:integer,public v2:integer,public c1:integer,public c2:integer].


Self <- create(V1, V2,C1,C2) :-
        Self << v1(V1),
        Self << v2(V2),
        Self << c1(C1),
        Self << c2(C2).


Self <- getConstraint(Matrix,Ctr,Rank) :-
                Self >> c1(Col1),
                Self >> c2(Col2),
                Self >> v1(V1),
                Self >> v2(V2),
                colonne(Matrix,Col1,C1),
                colonne(Matrix,Col2,C2),
                Ctr =[element(Rank,C1,V1),element(Rank,C2,V2)].


Self <- toScreen :-
        Self >> c1(Col1),
        Self >> c2(Col2),
        Self >> v1(V1),
        Self >> v2(V2),
        print('Pair : '),
        print('('),print(V1),print(','),print(V2),print(');('),print(Col1),print(','),print(Col2),print(')').

:-end_class.