:-use_module(library(objects)). 
:-use_module(library(lists)).
:-load_files(library(obj_decl),[when(compile_time), if(changed)]).
:- include(['matrix.pro','fm.pro','pair.pro']).


constraintGenerationTest:-
        Constraints = [and(A,[B,C])],
        FeatureList = [A,B,C],
        matrice(M,3,4),
        buildConstrainedMatrix(M,Constraints,FeatureList,MatrixConstraints),
        printMatrixScreen(M),
        buildValidPair([and(A,[B,C])],[A,B,C],A,R),
        println(MatrixConstraints),
        println(R).

testFm:-
        buildValidPair([and(A,[B,C])],[A,B,C],A,R),
        print(R).
        

      
testfor:-
        for(I,1,10)
        do
        (print(I)).  