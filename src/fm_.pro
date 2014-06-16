:- use_module(library(objects)). 
:-use_module(library(lists)).
:-use_module(library(samsort)).
:-use_module(library(atts)).
:-use_module(library(clpfd)).
:- load_files(library(obj_decl),[when(compile_time), if(changed)]).
:- include(['pair.pro']).




buildValidPair(Constraints, FeatureList, Root, Out) :-      
        length(FeatureList,L),
        (for(I,1,L-1), 
          fromto([],In,Out2,Out),
         param([L,Constraints, FeatureList, Root]) 
         do (for(J,I+1,L), 
             fromto(In,In2,Out2,Out2),
            param([I,Constraints, FeatureList, Root]) 
            do          
             buildPair(Constraints, FeatureList, Root,I,J,0,0,R1),
             buildPair(Constraints, FeatureList, Root,I,J,0,1,R2),
             buildPair(Constraints, FeatureList, Root,I,J,1,0,R3),
             buildPair(Constraints, FeatureList, Root,I,J,1,1,R4),
             append([R1,R2,R3,R4],R),
             append(In2,R,Out2))).

       
buildConstrainedMatrix(MatrixIn,Constraints,FeatureList,Res):-
        nbline(MatrixIn,NbLine),
       (for(I,1,NbLine),
        fromto([],In,Out,Res),
         param([MatrixIn,Constraints,FeatureList])
        do
        (line(MatrixIn,I,Line),
         copy_term((Constraints, FeatureList),  (Res, Line)),
         append(Res,In,Out))).     



println(A):-
        print(A),
        print('\n').


buildPair(Constraints, FeatureList, Root,C1,C2,V1,V2,[PairObj1]):-
        domain(FeatureList,0,1),
        nth1(C1,FeatureList,FT1),
        nth1(C2,FeatureList,FT2),
        \+(\+(callRec([Root#=1, FT1#=V1, FT2#=V2,callRec(Constraints),callRec(Constraints),callRec(Constraints),labeling([ff],FeatureList)]))),!,
        create(pair(V1,V2,C1,C2), PairObj1).

buildPair(_, _, _,_,_,_,_,[]).
/*
   Tranform input constraints in sicsuts constraints
   */
getConstraints([],[]).
getConstraints([and(A,L)|R],RR):-!,buildand(A,L,Res),getConstraints(R,R2),append(Res,R2,RR).
getConstraints([or(A,L)|R],RR):-!,buildor(A,L,Res),getConstraints(R,R2),append(Res,R2,RR).
getConstraints([xor(A,L)|R],RR):-!,buildxor(A,L,Res),getConstraints(R,R2),append(Res,R2,RR).
getConstraints([opt(A,L)|R],RR):-!,buildopt(A,L,Res),getConstraints(R,R2),append(Res,R2,RR).
getConstraints([Else|R],[Else|R2]):-!,getConstraints(R,R2).

buildopt(_,[],[]).
buildopt(A,[AA|R],[(A#=0)#=>(AA#=0)|RR]):-buildopt(A,R,RR).

buildand(_,[],[]).
buildand(A,[AA|R],[A#=AA|RR]):-buildand(A,R,RR).


buildxor(A,L,[sum(L,#= ,A)]).

buildor(A,L,[sum(L,#= ,R),(A#=0)#=>(R#=0),(A#=0)#=>(R#>0)]).



test(R):-List =[and(A_R,[A_R_1]),opt(A_R,[A_R_2,A_R_3,A_R_4,A_R_5,A_R_17]),opt(A_R_1,[A_R_1_7,A_R_1_8,A_R_1_9,A_R_1_31]),and(A_R_1,[A_R_1_13,A_R_1_22]),or(A_R_1_9,[A_R_1_9_10_11,A_R_1_9_10_12]),xor(A_R_1_13,[A_R_1_13_14_15,A_R_1_13_14_16]),or(A_R_1_31,[A_R_1_31_32_33,A_R_1_31_32_34,A_R_1_31_32_35,A_R_1_31_32_36]),opt(A_R_2,[A_R_2_6]),and(A_R_3,[A_R_3_25]),or(A_R_3_25,[A_R_3_25_26_27,A_R_3_25_26_28,A_R_3_25_26_29,A_R_3_25_26_30]),or(A_R_17,[A_R_17_19_20,A_R_17_19_21]), #\(A_R_4)  #\/  (A_R_3 ), #\(A_R_1_31)  #\/  (A_R_3 ),A_R= 1],getConstraints(List,R).


callRec([]).
callRec([A|T]) :-
        call(A),!,
        callRec(T).
