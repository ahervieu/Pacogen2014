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
/*
   
   
A call

    ?- ( fromto(From,In,Out,To) do Body ).

is translated into

    ?- do__1(From, To).

    do__1(Last, Last) :- !.
    do__1(In, Last) :- Body, do__1(Out, Last).
   
    
     
             param([I,MatrixIn,Constraints,FeatureList])
        do (
              println(I),
             line(MatrixIn,I,Line),
             println(Constraints),
             println(FeatureList),
             println(Line),
             copy_term((Constraints, FeatureList),  (Res, Line)),
              println(Res))).   
              
               for(I,1,10)
        do
        (print(I)).  
   */
       
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
        \+(\+(callRec([Root=1, FT1=V1, FT2=V2,callRec(Constraints),labeling([ff],FeatureList)]))),!,
        create(pair(V1,V2,C1,C2), PairObj1).

buildPair(_, _, _,_,_,_,_,[]).

/*
        create(pair(NIth,NJth,0,0), PairObj1),
                         create(pair(NIth,NJth,1,1), PairObj2),
                         create(pair(NIth,NJth,1,0), PairObj3),
                         create(pair(NIth,NJth,0,1), PairObj4),
                         PairObj1 <- toScreen,
                         PairObj2 <- toScreen,
                         PairObj3 <- toScreen,
                         PairObj4 <- toScreen*/
/*
GlobalConstraint
*/


        clpfd:dispatch_global(and(P,LF), state(_), state(_), Actions) :-
            and_solver(P,  LF,Actions).  

      clpfd:dispatch_global(or(P,_), state(LF0), state(LF), Actions) :-
            or_solver(P, LF0, LF,Actions).    

        clpfd:dispatch_global(xor(P,_), state(LF0), state(LF), Actions) :-
            xor_solver(P, LF0, LF,Actions). 

        clpfd:dispatch_global(mand(P,LF), state(_), state(_), Actions) :-
            mand_solver(P,  LF,Actions).

        clpfd:dispatch_global(opt(P,LF), state(_), state(_), Actions) :-
             opt_solver(P,  LF,Actions).

       clpfd: dispatch_global(card(Min,Max,P,LF),state(_),state(_),Actions):-
             card_solver(Min,Max,P,LF,Actions).

        clpfd:dispatch_global(require(A,B), state(_), state(_), Actions) :-
            require_solver(A, B,Actions).

        clpfd:dispatch_global(mutex(A,B), state(_), state(_), Actions) :-
            mutex_solver(A,B,Actions).

        clpfd:dispatch_global(cnf(A,B), state(_), state(_), Actions) :-
            cnf_solver(A,B,Actions).
                        
        

        cnf(A,B):- 
                dom_suspensions(A,Susp),
                dom_suspensions(B,Susp2),
                append(Susp,Susp2,SuspS),
                fd_global(cnf(A,B), state(_), SuspS).

/*
Compte le nolmbre d'occurence de -1 et de 0, rend la liste des variables non instanciées
       N : nbre de valeur supérieure à 0
       M : nbre de 0
                    filter(L,LFLibre,N,M)
*/
/*
A : neg
B : not neg   
-*/

        cnf_solver(A,B,Actions):-
               filter(A,A_Free,NA,MA),
               filter(B,B_Free,NB,MB),
               length(A,LA),
               length(B,LB),
    
               (
                  /* cases where B = []) */
                  LB == 0, MA > 0 -> Actions =  [exit] ;
                  LB == 0,NA is LA - 1 -> setVal(0,A_Free,R),Actions =  [exit| R] ;  
                  
                  MB \= 0,MB == LB ->  setVal(0,A,R),Actions =  [exit| R] ;
                  MA > 0 -> Actions = [exit] ;
                  NB > 0 -> Actions = [exit] ;
                  NA == LA, MB is LB - 1 -> setVal(1,B_Free,R),Actions =  [exit| R] ;
                  MB == LB, NA is LA - 1 -> setVal(0,A_Free,R),Actions =  [exit| R] ;
                  Actions = []
               
                  ).

                  
               
                  require_solver(A, B,Actions) :-
               (
                B == 0 -> Actions =  [exit, A = 0];   
                B == 1 -> Actions =  [exit];     
                A == 0 -> Actions =  [exit];   
                A == 1 -> Actions =  [exit, B = 1];       
                Actions = []
              ).      
               
               
callRec([]).
callRec([A|T]) :-
        call(A),
        callRec(T).            



        and(P,LF):-
              dom_suspensions(LF,Susp),Susp2 = [val(P),min(P)|Susp],
              fd_global(and(P,LF), state(_), Susp2).

  

         and_solver(P,LF,Actions) :-
                filter(LF,_,N,M),

             ( M > 0                   -> Actions = [exit |Ps], ex_eq([P |LF],0,Ps)
             ; P == 0                 -> Actions = [exit |Ps], ex_eq(LF,0,Ps)
             ; fd_min(P,K), K > 0     -> Actions = [exit |Ps],ex_neq(LF,0,Ps)
             ; N > 0                   -> Actions = [exit |Ps], ex_neq([P |LF],0,Ps)
             ; Actions = []
             ).
          



        or(P,LF):-       
              dom_suspensions(LF,Susp),Susp2 = [min(P),val(P)|Susp],
              fd_global(or(P,LF), state(LF), Susp2).

        
         or_solver(P, LF0, LF,Actions) :-
               filter(LF0,LF,N,M),
               length(LF0,L),/* N : nbre de 1 dans la liste */
         
             ( P == 0                                              -> Actions = [exit |Ps],ex_eq(LF0,0,Ps)
             ; fd_min(P,K), K > 0, N > 1                           -> Actions = [exit]
             ; fd_min(P,K), K > 0,L2 is  L - 1 ,M == L2, N==0      -> LF = [A],Actions = [exit ,A = 1]
             ; N > 0                                                -> Actions = [exit |Ps1],ex_neq([P], 0, Ps1)
             ; M == L                                               -> Actions = [exit |Ps1],ex_eq([P], 0, Ps1)
             ; Actions = []
             ).
          

      

        xor(P,LF):-
              dom_suspensions(LF,Susp),Susp2 = [min(P),val(P)|Susp],
              fd_global(xor(P,LF), state(LF), Susp2).
         xor_solver(P, LF0, LF,Actions) :-
               filter(LF0,LF,N,M) ,
                 length(LF0,L),/* N : nbre de 0 dans la liste , M nbre de - 1*/
           
             ( P == 0                                  -> Actions = [exit |Ps], ex_eq(LF0,0,Ps)
             ; N > 1                                    -> Actions = [fail]
             ; N == 1                                   -> Actions = [exit |Ps3],ex_neq([P], 0, Ps2), ex_eq(LF, 0, Ps),append(Ps,Ps2,Ps3) /* tous les éléments de la liste sont égaux à -1 */
             ; L2 is L-1, M == L2, fd_min(P,K), K > 0  -> Actions = [exit |Ps3],ex_neq([P], 0, Ps2), ex_neq(LF, 0, Ps),append(Ps,Ps2,Ps3) 
             ; M == L                                   -> Actions = [exit |Ps1], fdset_singleton(Set, 0),Ps1 = [P in_set Set]
             ; Actions = []
             ).





       mand(P,LF):-
              dom_suspensions(LF,Susp),Susp2 = [min(P),val(P)|Susp],
              fd_global(mand(P,LF), state(_), Susp2).

        mand_solver(P, LF,Actions) :-
                filter(LF,_,N,M),

             ( M > 0                   -> Actions = [exit |Ps], ex_eq([P |LF],0,Ps)
             ; P == 0                 -> Actions = [exit |Ps], ex_eq(LF,0,Ps)
             ; fd_min(P,K), K > -1     -> Actions = [exit |Ps],ex_neq(LF,0,Ps)
             ; N > 0                   -> Actions = [exit |Ps], ex_neq([P |LF],0,Ps)
             ; Actions = []
             ).


        mutex(A,B):-fd_global(mutex(A,B), state(_), [dom(A),dom(B)]).
        
        mutex_solver(A,B, Actions) :-
                (
                A == 1 ->Actions = [exit, call(B = 0)] ;
                A == 0 ->Actions = [exit] ;
                B == 1 ->Actions = [exit, call(A = 1)] ;
                B == 0 ->Actions = [exit] ;
                Actions = []
                ).


       opt(P,LF):-
              dom_suspensions(LF,Susp),Susp2 = [val(P), min(P)|Susp],
              fd_global(opt(P,LF), state(_), Susp2).

        opt_solver(P, LF,Actions) :-       
              filter(LF,_,N,_),        
             ( N > 0                 ->  Actions = [exit|Ps], ex_neq([P],0,Ps)
             ; P == 0               ->  Actions = [exit|Ps], ex_eq(LF,0,Ps)
             ; fd_min(P,K), K > 0   ->  Actions = [exit]
             ; Actions = []
             ).



       

        card(Min,Max,P,LF):-
             dom_suspensions(LF,Susp),Susp2 = [val(P)|Susp],
             fd_global(card(Min,Max,P,LF), state(LF), Susp2).

        card_solver(Min,Max,P, LF,Actions) :-
                filter(LF,LF2,N,_),
                (
                 P == 0                           -> Actions = [exit|Ps], ex_eq(LF,0,Ps);
                 P == 1, N > Max                  -> Actions = [fail];
                 P == 1, N < Min, D is Min - N,length(LF2,A), D > A 
                                                  -> Actions = [fail];
                 P == 1, N == Max                 -> Actions = [exit|Ps], ex_eq(LF2,0,Ps);
                 P == 1, N < Min, D is Min - N,length(LF2,D) 
                                                  -> Actions = [exit|Ps], ex_eq(LF2,1,Ps);
                 Actions = []
                 ).



        

         require(A,B):-
             fd_global(require(A,B), state(_), [dom(A),dom(B)]).
/*
A => B
*/






        filter([],[],0,0).
/*
Compte le nolmbre d'occurence de -1 et de 0, rend la liste des variables non instanciées
       N : nbre de valeur supérieure à -1
       M : nbre de -1
                    filter(L,LFLibre,N,M)
*/
        filter([LF|LFs],LF0,N,M) :- 
                 fd_min(LF,K), K > 0 ,!,
                 filter(LFs,LF0,N1,M), N is N1 +1 .
               
     
        filter([LF|LFs],LF0,N,M) :-  
                LF == 0,!,
               filter(LFs,LF0,N,M1), M is M1 +1 .

        filter([LF|LFs],LF0,N,M):-
                LF0 = [LF|LF02],
                filter(LFs,LF02,N,M).



                

                       
      

        copy([],[]).
        copy([A|R],[A|R2]):- 
                copy(R,R2).

      dom_suspensions([], []).
     dom_suspensions([X|Xs], [dom(X)|Susp]) :-
             dom_suspensions(Xs, Susp).

           

                       


% exactly.pl
     

     % rules [1,2]: filter the X's, decrementing N
     ex_filter([], [], N, N, _).
     ex_filter([X|Xs], Ys, L, N, I) :- X==I, !,
             M is L-1,
             ex_filter(Xs, Ys, M, N, I).
     ex_filter([X|Xs], Ys0, L, N, I) :-
             fd_set(X, Set),
             fdset_member(I, Set), !,
             Ys0 = [X|Ys],
             ex_filter(Xs, Ys, L, N, I).
     ex_filter([_|Xs], Ys, L, N, I) :-
             ex_filter(Xs, Ys, L, N, I).
     
     % rule [3]: all must be neq I
     ex_neq(Xs, I, Ps) :-
             fdset_singleton(Set0, I),
             fdset_complement(Set0, Set),
             eq_all(Xs, Set, Ps).
     
     % rule [4]: all must be eq I
     ex_eq(Xs, I, Ps) :-
             fdset_singleton(Set, I),
             eq_all(Xs, Set, Ps).
     
     eq_all([], _, []).
     eq_all([X|Xs], Set, [X in_set Set|Ps]) :-
             eq_all(Xs, Set, Ps).

      contrainte(A,B,L1,L2,R)  :-
             dom_suspensions2(L1,L2, Susp),
             fd_global(contrainte(A,B,L1,L2,R) , state(L1,L2), Susp).
     
     dom_suspensions2([], [], []).
     dom_suspensions2([XA|XAs],[XB|XBs], [dom(XA),dom(XB)|Susp]) :-
             dom_suspensions2(XAs,XBs, Susp).

