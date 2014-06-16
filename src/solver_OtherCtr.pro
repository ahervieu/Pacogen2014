:- use_module(library(objects)). 
:-use_module(library(lists)).
:-use_module(library(clpfd)).
:- load_files(library(obj_decl),[when(compile_time), if(changed)]).
:- include(['matrix.pro','fm_.pro']).


openfile2(Features,CtrList,Labelling,MatrixSize,ModelName,FileName) :-
        open('model.txt', read, Stream),
        read(Stream,[Features,CtrList,Labelling,MatrixSize,ModelName,FileName]),close(Stream).

run:-
       openfile2(Features,ConstraintModel,Labeling,MatrixSize,ModelName,FileName),
     solverMin(Features,ConstraintModel,MatrixSize,Labeling,ModelName,FileName).

save:-save_program('/Users/Aymeric/Documents/runtime_V2/Pacogen.sav').

user:runtime_entry(start) :-
        openfile(PairInformations,Attributes,Features,ConstraintModel,MatrixSize,ModelName,Labeling,Minimization,AllDif,Sort,SymetricBreaking,FileName),
        solverMin(Features,ConstraintModel,MatrixSize,Labeling,ModelName,FileName).

solver(Features,ConstraintModel2,MatrixSize,Labeling,ModelName,FileName):-
          getConstraints(ConstraintModel2,ConstraintModel),
          callRec(ConstraintModel),
          callRec(ConstraintModel),
          callRec(ConstraintModel),
         statistics(runtime, [T0|_]),
        Features = [Root|_],
        borneRec(Features),
        length(Features,L),
        matrice(Matrix,L,MatrixSize),
        variable(Matrix,VarList),
        borneRec(VarList),   
        buildConstrainedMatrix(Matrix,ConstraintModel,Features,MatrixConstraints),
         printMatrixScreen(Matrix),
        println(MatrixConstraints),
        callRec(MatrixConstraints),
        callRec(MatrixConstraints),
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
          printMatrixScreen(Matrix),
        writeStat('stats.txt',FileName,ModelName,Max,T,Labeling).
        


solverMin(Features,ConstraintModel2,MatrixSize,Labeling,ModelName,FileName):-
        getConstraints(ConstraintModel2,ConstraintModel),
        callRec(ConstraintModel),
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

solverMin2(Features,ConstraintModel2,MatrixSize2,Labeling,ModelName,FileName,TimeOut):-
     getConstraints(ConstraintModel2,ConstraintModel),
          callRec(ConstraintModel),
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
        

simpleTest:-solver([A,A1,A2,A3,B,B1,B2,B3,R],[and(R,[A,B]),or(A,[A1,A2,A3]),or(B,[B1,B2,B3]),R=1],20,[ff],t,test).

debut(FT):- FT =[AINSURED_OBJECT,ACORPORATION,AREALTY,AMOVABLE_PROPERTY,APERSON,ACOVERAGE,AILLNESS,ALIFE,AUNEMPLOYMENT,ADAMAGE,ALOSS,APAYMENT,ASERVICE,AAMMOUNT,AOWN_RISK,ACONDITIONS,AACCEPTANCE,AEXCEPTION,APREMIUM,ADIRECT,APERIODICAL,APAYEE,A_PERSON,APAYEE_CORPORATION,AINSURANCE_PRODUCT],
        Ctr =[xor(AINSURED_OBJECT,[ACORPORATION,AREALTY,AMOVABLE_PROPERTY,APERSON]),or(ACOVERAGE,[AILLNESS,ALIFE,AUNEMPLOYMENT,ADAMAGE,ALOSS]),xor(APAYMENT,[ASERVICE,AAMMOUNT]),opt(APAYMENT,[AOWN_RISK]),and(ACONDITIONS,[AACCEPTANCE,AEXCEPTION]),xor(APREMIUM,[ADIRECT,APERIODICAL]),or(APAYEE,[A_PERSON,APAYEE_CORPORATION]),and(AINSURANCE_PRODUCT,[AINSURED_OBJECT,ACOVERAGE,APAYMENT,ACONDITIONS,APREMIUM,APAYEE]),AINSURANCE_PRODUCT#=1, (#\(ACORPORATION) #\/  #\(AILLNESS)) #/\( #\(APERSON) #\/  #\(ADAMAGE)) #/\ (#\(ALOSS)  #\/  (AMOVABLE_PROPERTY ))#/\ (#\(ACORPORATION)  #\/  (APAYEE_CORPORATION ))],
        domain(FT,0,1),
          getConstraints(Ctr,ConstraintModel),
          callRec(ConstraintModel),
        callRec(ConstraintModel),
        A_PERSON #=1, ALOSS#=1,labeling([ff],FT).
nbconfig:- openfile(PairInformations,Attributes,Features,ConstraintModel,MatrixSize,ModelName,Labeling,Minimization,AllDif,Sort,SymetricBreaking,FileName),
        assert((t(0))),
        open('nbConfig.txt', append, Stream),write(Stream,Etiquette),write(Stream,';'),close(Stream),
        allconfig(Features,Features, ConstraintModel),
        retract((t(L))),
        open('nbConfig.txt', append, Stream2),write(Stream2,L),nl(Stream2),close(Stream2),
        write(' L : '),writeln(L), halt.
        

lab(FeaturesList) :-      
        labeling([],FeaturesList),
        retract((t(N))),
        N1 is N+1,
        asserta((t(N1))).


  allconfig(FeaturesList,PrimitivesFeaturesList, Constraints) :-
         domain(FeaturesList,0,1),
          getConstraints(Constraints,ConstraintModel2),
          callRec(ConstraintModel2),

      findall(_,lab(FeaturesList),_).

        
borneRec([A|R]):-    
       A in 0..1,
       borneRec(R).
borneRec([]).