:- use_module(library(objects)). 
:-use_module(library(lists)).
:-use_module(library(clpfd)).
:- load_files(library(obj_decl),[when(compile_time), if(changed)]).
:- include(['matrix.pro','fm.pro']).
openfile2(Features,CtrList,Labelling,MatrixSize,ModelName,FileName) :-
        open('model.txt', read, Stream),
        read(Stream,[Features,CtrList,Labelling,MatrixSize,ModelName,FileName]),close(Stream).


openfile(PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,Labelling,Minimization,AllDif,Sort,SymetricBreaking,FileName) :-
        open('model.txt', read, Stream),
        read(Stream,[PairInformations,Attributes,Features,CtrList,MatrixSize,ModelName,Labelling,Minimization,AllDif,Sort,SymetricBreaking,FileName]),close(Stream).

run:-
       openfile2(Features,ConstraintModel,Labeling,MatrixSize,ModelName,FileName),
     solverMin(Features,ConstraintModel,MatrixSize,Labeling,ModelName,FileName).

save:-save_program('/Users/Aymeric/Documents/Thèse/Expériences Pacogen/runtime/Pacogen.sav').

user:runtime_entry(start) :-
        openfile(PairInformations,Attributes,Features,ConstraintModel,MatrixSize,ModelName,Labeling,Minimization,AllDif,Sort,SymetricBreaking,FileName),
solverMin(Features,ConstraintModel,MatrixSize,Labeling,ModelName,FileName).

solver(Features,ConstraintModel,MatrixSize,Labeling,ModelName,FileName):-
         statistics(runtime, [T0|_]),
        Features = [Root|_],
        borneRec(Features),
        length(Features,L),
        matrice(Matrix,L,MatrixSize),
        variable(Matrix,VarList),
        borneRec(VarList),   
        buildConstrainedMatrix(Matrix,ConstraintModel,Features,MatrixConstraints),
        callRec(MatrixConstraints),
        buildValidPair(ConstraintModel,Features,Root,PwCtr),
        (foreach(P,PwCtr),
        fromto([],In,RkLst,RkLst), 
        param(Matrix)
        do (P <-getConstraint(Matrix,Ctr,Rank),
            callRec(Ctr),
            RkLst =[Rank|In])),
        domain(RkLst,1,MatrixSize),
        maximum(Max,RkLst),
        println('Solving'),
        labeling(Labeling,RkLst),
        println(Max), statistics(runtime, [T1|_]),
        T is T1 - T0,
        print(T),println(' ms'),
        writeStat('stats.txt',FileName,ModelName,Max,T,Labeling).
        


solverMin(Features,ConstraintModel,MatrixSize,Labeling,ModelName,FileName):-
        println('First resolution'),
        statistics(runtime, [T0|_]),
        Features = [Root|_],
        borneRec(Features),
        length(Features,L),
        matrice(Matrix,L,MatrixSize),
        variable(Matrix,VarList),
        borneRec(VarList),   
        buildConstrainedMatrix(Matrix,ConstraintModel,Features,MatrixConstraints),
        callRec(MatrixConstraints),
        buildValidPair(ConstraintModel,Features,Root,PwCtr),
        (foreach(P,PwCtr),
        fromto([],In,RkLst,RkLst), 
        param(Matrix)
        do (P <-getConstraint(Matrix,Ctr,Rank),
            callRec(Ctr),
            RkLst =[Rank|In])),
        domain(RkLst,1,MatrixSize),
        maximum(Max,RkLst),
        println('Solving'),
        labeling(Labeling,RkLst),
        print('solution '),
        println(Max), 
        statistics(runtime, [T1|_]),
        T is T1 - T0,
        print(T),println(' ms'),
        printMatrixScreen(Matrix),
         writeMatrixFile('res.csv',Matrix),
        writeStat('stats.txt',FileName,ModelName,Max,T,Labeling),!,
        solverMin2(Features,ConstraintModel,Max,Labeling,ModelName,FileName,T).

solverMin2(Features,ConstraintModel,MatrixSize2,Labeling,ModelName,FileName,TimeOut):-
        MatrixSize is MatrixSize2 -1,      
        println('other resolution'),
        print('MatirxInput : '),        println(MatrixSize2),
        print('MatrixInputResoltion : '),        println(MatrixSize),
        print('Allocated Time (*4)'),        println(TimeOut),
        statistics(runtime, [T0|_]),
        Features = [Root|_],
        borneRec(Features),
        length(Features,L),
        matrice(Matrix,L,MatrixSize),
        variable(Matrix,VarList),
        borneRec(VarList),   
        buildConstrainedMatrix(Matrix,ConstraintModel,Features,MatrixConstraints),
        callRec(MatrixConstraints),
        buildValidPair(ConstraintModel,Features,Root,PwCtr),
        (foreach(P,PwCtr),
        fromto([],In,RkLst,RkLst), 
        param(Matrix)
        do (P <-getConstraint(Matrix,Ctr,Rank),
            callRec(Ctr),
            RkLst =[Rank|In])),
        domain(RkLst,1,MatrixSize),
        maximum(Max,RkLst),
        println('Solving'),
        Time is TimeOut * 10,
        labeling([time_out(Time,F)|Labeling],RkLst),!,
        ( (F = time_out) ->
          writeStat('statsF.txt',FileName,ModelName,MatrixSize2,TimeOut,Labeling) ;
        print('solution size'),
        println(Max), 
        statistics(runtime, [T1|_]),
        T is T1 - T0,
        print(T),println(' ms'),
           printMatrixScreen(Matrix),
         writeMatrixFile('res.csv',Matrix),
        writeStat('stats.txt',FileName,ModelName,Max,T,Labeling),
        solverMin2(Features,ConstraintModel,Max,Labeling,ModelName,FileName,T)).


 writeStat(File,ModelFileName,ModelName,NumberOfConfig,Time,Labeling):-
        open(File, append, Stream),
        write(Stream,ModelFileName),
        write(Stream,';'),
        write(Stream,ModelName),
        write(Stream,';'),
        write(Stream,Labeling),
        write(Stream,';'),
        write(Stream,NumberOfConfig),
        write(Stream,';'),
        write(Stream,Time),
        nl(Stream),
        close(Stream).       
        
        
borneRec([A|R]):-    
       A in 0..1,
       borneRec(R).
borneRec([]).