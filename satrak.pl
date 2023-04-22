:- use_module(library(lists)).

% :- type fLeiro  ---> satrak(sSzS, sSzO, list(parc)).
% :- type sSzS    == list(int).
% :- type sSzO    == list(int).
% :- type parc    == int-int.
% :- type irany   ---> n    % észak
%                 ;    e    % kelet
%                 ;    s    % dél
%                 ;    w.   % nyugat
% :- type sHelyek   == list(irany).
% :- pred satrak(fLeiro::in, sHelyek::out).


% az elején lefuttatom rekurzivan khf 6,5öt 1x minden fára/oszlopra/sorra, majd minden fa maradék irányán rekurzivan végig megyek
% ahol mindig a khf6, 5 öt lefuttatom mindig
% ellenőrzöm h lehet e tovább menni majd tovább megyek
%végén ellenörzőm h jó e a megoldás

satrak(satrak(Ss, Os, Fs),SHelyek):-
   	lista_hossz(Ss,SorHossz),
    lista_hossz(Os,OszHossz),
  	iranylistak(Ss,Os,SorHossz-OszHossz,Fs,Iranyok),
    iranyok_koordinatai(Fs,Iranyok,IranyKoord),
    rekurzivkhf5_6(Fs,Ss,Os,SorHossz,OszHossz,Iranyok,IranyKoord,UjIranyok,UjIranyKoord),
    [HIR|TIR]= UjIranyok,
    [HIK|TIK]= UjIranyKoord,
    megoldas(Ss,Os,Fs,HIR,TIR,HIK,TIK,Temp),
    member(SHelyek,Temp).

% :- pred rekurzivkhf5_6(fLeiro::in, sSzS::in, sSzO::in, int::in, int::in, list(irany)::in, list(parc)::in, list(irany)::out, list(parc)::out).
rekurzivkhf5_6(Fs,Ss,Os,SorHossz,OszHossz,Iranyok,IranyKoord,ErIrany,ErIk):-
     (   osz_sor_szukites(Ss,Os,SorHossz,OszHossz,Iranyok,IranyKoord,SzukitettIrany,SzukitettIranyKoord)->true
     ;   
     	SzukitettIrany=Iranyok,
         SzukitettIranyKoord=IranyKoord     
     ),
    (   sator_szukites(Fs,SzukitettIrany,SzukitettIranyKoord,UjIranyok,UjIranyKoord)->
    	true
    ;
    	UjIranyok=SzukitettIrany,
        UjIranyKoord=SzukitettIranyKoord
    ),
    ( Iranyok=UjIranyok ->  ErIrany=Iranyok,ErIk=IranyKoord
    ;
    	rekurzivkhf5_6(Fs,Ss,Os,SorHossz,OszHossz,UjIranyok,UjIranyKoord,ErIrany,ErIk)
    )
.
% meghivom a khf6ot az összes sorra és oszlopra
% :- pred osz_sor_szukites(list(int)::in, list(int)::in, int::in, int::in, list(irany)::in, list(int-int)::in, list(irany)::out, list(int-int)::out).
osz_sor_szukites(Ss,Os,SorHossz,OszHossz,Iranyok,IranyKoord,ErIrany,ErIk):-
    sorVegig(SorHossz,1,Ss,Iranyok,IranyKoord,Iranyok2,IranyKoord2),
    oszlopVegig(OszHossz,1,Os,Iranyok2,IranyKoord2,ErIrany,ErIk).

% khf 6 osszes soron
% :- pred sorVegig(int::in, int::in, list(int-int)::in, list(irany)::in, list(int-int)::in, list(irany)::out, list(int-int)::out).
sorVegig(SorHossz,Iter,[HSor|TSor],Irany,IranyKoord,Iranyok2,IranyKoord2):-
    UjIter is Iter+1,
    (   HSor>0 ->  
    osszeg_szukites(sor(Iter,HSor),IranyKoord,Irany,UjIrany,UIK),    
      (   Iter=\=SorHossz->  sorVegig(SorHossz,UjIter,TSor,UjIrany,UIK,Iranyok2,IranyKoord2);
          Iranyok2=UjIrany,
          IranyKoord2=UIK
      )
    ;
      (   Iter=\=SorHossz->  sorVegig(SorHossz,UjIter,TSor,Irany,IranyKoord,Iranyok2,IranyKoord2);
          Iranyok2=Irany,
          IranyKoord2=IranyKoord
      )
    ).
% khf 6 osszes oszlopon
% :- pred oszlopVegig(int::in, int::in, list(int-int)::in, list(irany)::in, list(int-int)::in, list(irany)::out, list(int-int)::out).
oszlopVegig(SorHossz,Iter,[HSor|TSor],Irany,IranyKoord,Iranyok2,IranyKoord2):-
    UjIter is Iter+1,
    (   HSor>0 ->  
      osszeg_szukites(oszl(Iter,HSor),IranyKoord,Irany,UjIrany,UIK),    
      (   Iter=\=SorHossz->  oszlopVegig(SorHossz,UjIter,TSor,UjIrany,UIK,Iranyok2,IranyKoord2);
          Iranyok2=UjIrany,
          IranyKoord2=UIK
      )
    ;
      (   Iter=\=SorHossz->  oszlopVegig(SorHossz,UjIter,TSor,Irany,IranyKoord,Iranyok2,IranyKoord2);
          Iranyok2=Irany,
          IranyKoord2=IranyKoord
      )
    ).
    

% ellenorzi hogy maradt-e pozitiv értékü sor v oszlop
% :- pred checkSorVOszlop(list(int)::in, int::out)
checkSorVOszlop([],0).
checkSorVOszlop([H|T],Eredm):-
    (   H>0 ->   Eredm=1; checkSorVOszlop(T,Eredm)).


% egy fa iranyain ha vegig er
megoldas(_,_,_,[],_,[],_,[]).
% vegig megy egy fa iranyain 
% :- pred megoldas(int::in, int::in, list(int-int)::in, list(int-int)::in, list(int)::in, list(irany)::in, list(irany)::in, list(int-int)::in, list(int-int)::in, list(list(irany))::out).
megoldas(Ss,Os,Fs,[IRH|IRT],TIR,[IKH|IKT],TIK,EgyMegoldas):-
    
    (   szukit_egy_iranyra(Ss,Os,Fs,[IRH|TIR],[IKH|TIK],EgyM)->append(EgyM,SokM,EgyMegoldas);EgyMegoldas=SokM),
    %maradek iranyon megy vegig
    megoldas(Ss,Os,Fs,IRT,TIR,IKT,TIK,SokM).

% amikor a fak vegehez ért
megoldas(Ss,Os,[],[],[],[],[],EgyMegoldas,1):-
    checkSorVOszlop(Ss,Sor),
    checkSorVOszlop(Os,Oszlop),
     (   Sor=:=0 ->  (   Oszlop=:=0 ->  EgyMegoldas=0;EgyMegoldas=1 );EgyMegoldas=1).

% nem 0 a keresett oszlop vagy sor
% :- pred rakhatoSorVOszlop(int::in,int::in,list(int)::in,int::out).
rakhatoSorVOszlop(Keresett,Iter,[HLista|TLista],Er):-
    (   Iter=\=Keresett ->  UjIter is Iter+1, rakhatoSorVOszlop(Keresett,UjIter,TLista,Er);
    	(   HLista==0 ->  fail;Er=HLista)
    ).
    
% :- vanElegIrany(list(int)::in,int::in)
% maradt eleg sátor a pozitiv értékű sorokhoz vagy oszlopokhoz
vanElegIrany([],Db):-
    (   Db<0 ->  fail;true)
.
vanElegIrany([Hs|Ts],Db):-
    (   Hs>0 ->  UjDb is Db-Hs, vanElegIrany(Ts,UjDb);
    vanElegIrany(Ts,Db)
    )
.

% a kivalasztott irányt leellenörzi majd továb hiv a kövi fa irányaira ha ez az irány jó
% minden 10. fánál a khf5,6ot is lefuttatom illetve ellenőrzöm ,hogy
%  elég sátor maradt e az oszlopok/sorok megoldásához
% :- pred szukit_egy_iranyra(int::in, int::in, list(int-int)::in, list(int-int)::in, list(int)::in, list(irany)::in, list(int-int)::in, list(list(irany))::out).
szukit_egy_iranyra(Ss,Os,[HS|TS],[HIR|TIR],[HIK|TIK],EgyMegoldas):-
   X=[HIR],
   Ik=[HIK],
   [Sor-Osz]=Ik,
   rakhatoSorVOszlop(Sor,1,Ss,SorDb),
   rakhatoSorVOszlop(Osz,1,Os,OszDb),
   (   
   		lista_hossz(Ss,SorHossz),
    	lista_hossz(Os,OszHossz),
        lista_hossz(TS,MarSator), 
   		MaradekSator is MarSator+1,
        vanElegIrany(Ss,MaradekSator),
        vanElegIrany(Os,MaradekSator),
       	osz_sor_szukites(Ss,Os,SorHossz,OszHossz,[X|TIR],[Ik|TIK],SzukitettIrany,SzukitettIranyKoord),
       	sator_szukites([HS|TS],SzukitettIrany,SzukitettIranyKoord,MegaIrany,MegaIK)
   ),
    (   TIR\= [] ->  
   	
   	
    (   masodik_sator_szukites( [HS|TS],MegaIrany,MegaIK,UjIrany3,UIK3) ->  
        (   SorDb > 0 ->  lista_csokkent(1,Ss,UjSs,Sor);UjSs=Ss),
        (   OszDb> 0 ->   lista_csokkent(1,Os,UjOs,Osz);UjOs=Os),
        [_|TUjIr]=UjIrany3,
        [_|TUjIk]=UIK3,
        [HUR|TUR]=TUjIr,
        [HUK|TUK]=TUjIk,
        megoldas(UjSs,UjOs,TS,HUR,TUR,HUK,TUK,UjMegoldas),
            (   UjMegoldas\=[] -> rekurzivanEleFuz(X,UjMegoldas,EgyMegoldas);EgyMegoldas=[])
            ;EgyMegoldas=[] 
    )
    ;
    	(   SorDb > 0 ->  lista_csokkent(1,Ss,UjSs,Sor);UjSs=Ss),
        (   OszDb> 0 ->   lista_csokkent(1,Os,UjOs,Osz);UjOs=Os),
    	megoldas(UjSs,UjOs,[],[],[],[],[],UjMegoldas,1),
            (   UjMegoldas=:=0 ->  EgyMegoldas=[X];EgyMegoldas=[])
    ).
    

% rekurzivan a mindegyik eredmény tömb elé fűz egy irányt
% :- pred rekurzivanEleFuz(irany::in,list(list(irany))::in,list(list(irany))::out).
rekurzivanEleFuz(_,[],[]).
rekurzivanEleFuz(Mit,[HLista|TLista],Eredm):-
    append(Mit,HLista,EgyLista),
    Eredm=[EgyLista|Maradek],
    rekurzivanEleFuz(Mit,TLista,Maradek).

% :- iranylistak(list(int)::in,list(int)::in,int-int::in,list(int-int)::in,list(irany)::out)
iranylistak(Ss,Os,NM,Fs,ILs):-
    Sor-Oszlop=NM,
    Soruj is Sor+1,
    Oszlopuj is Oszlop+1,
    calc_irany_lista(Ss,Os,Soruj-Oszlopuj,Fs,Fs,Temp),
    (   member([],Temp)->fail;ILs=Temp).

calc_irany_lista(_,_,_,[],_,[]).
% :- calc_irany_lista(list(int)::in,list(int)::in,int-int::in,list(int-int)::in,list(int-int)::in,list(irany)::out).
% egy fa iranyait kiszamolja majd a tobbi faet
calc_irany_lista(Ss,Os,NM,[HS|TS],Fs,ILs):-
    ILs = [EgyFa|Maradek],
    egy_fa_iranyai(Ss,Os,NM,HS,Fs,EgyFa),
    calc_irany_lista(Ss,Os,NM,TS,Fs,Maradek).
% :- pred van_e_fa(int::in,int::in,list(int-int)::in,int::out).
van_e_fa(_,_,[],Eredm):-Eredm=0.
van_e_fa(SOR,OSZLOP,[SOR-OSZLOP|_],Eredm):-
	    Eredm=1.
van_e_fa(SOR,OSZLOP,[X-Y|T],Eredm):-
    (SOR=\=X -> van_e_fa(SOR,OSZLOP,T,Eredm);
    ( OSZLOP =\=Y -> van_e_fa(SOR,OSZLOP,T,Eredm))).
% :- pred lehet_e_fa(list(int)::in,list(int)::in,int-int::in).
lehet_e_fa(Ss,Os,N-M):-
    Sor is N-1,
    nth0(Sor,Ss,SorDb),
    (   SorDb==0 ->  fail; 
    	Oszlop is M-1,
    	nth0(Oszlop,Os,OszDb),
      (   OszDb==0->  fail;
      true
      )
    )
    ,!.

% :- pred egy_fa_iranyai(list(int)::in,list(int)::in,int-int::in,int-int::in,list(int-int)::in,list(irany)::out).
egy_fa_iranyai(Ss,Os,NM,HS,Fs,Eredm):-
    egy_fa_iranyai(Ss,Os,NM,HS,Fs,Eredm,e).

% :- pred egy_fa_iranyai(list(int)::in,list(int)::in,int-int::in,int-int::in,list(int-int)::in,list(irany)::out,irany::in).
egy_fa_iranyai(Ss,Os,Sor-Oszlop,FaSor-FaOszlop,Fs,Eredm,e):-
    egy_fa_iranyai(Ss,Os,Sor-Oszlop,FaSor-FaOszlop,Fs,Tovabbi,n),
    UjFaOszlop is FaOszlop+1,
	    (   lehet_e_fa(Ss,Os,FaSor-UjFaOszlop) -> 
        van_e_fa(FaSor,UjFaOszlop,Fs,VanIttFa),
    	(   UjFaOszlop =\=Oszlop->listahoz_fuz(VanIttFa,e,Tovabbi,Eredm); Eredm=Tovabbi)
        ;
        Eredm=Tovabbi
        )
    .
    
egy_fa_iranyai(Ss,Os,Sor-Oszlop,FaSor-FaOszlop,Fs,Eredm,n):-
    egy_fa_iranyai(Ss,Os,Sor-Oszlop,FaSor-FaOszlop,Fs,Tovabbi,s),
    UjFaSor is FaSor-1,
    (   lehet_e_fa(Ss,Os,UjFaSor-FaOszlop) -> 
     van_e_fa(UjFaSor,FaOszlop,Fs,VanIttFa),
    (   UjFaSor =\=0->listahoz_fuz(VanIttFa,n,Tovabbi,Eredm); Eredm=Tovabbi)
    ;Eredm=Tovabbi)
   .

egy_fa_iranyai(Ss,Os,Sor-Oszlop,FaSor-FaOszlop,Fs,Eredm,s):-
    egy_fa_iranyai(Ss,Os,Sor-Oszlop,FaSor-FaOszlop,Fs,Tovabbi,w),
    UjFaSor is FaSor+1,
    (   lehet_e_fa(Ss,Os,UjFaSor-FaOszlop) -> 
    van_e_fa(UjFaSor,FaOszlop,Fs,VanIttFa),
    (   UjFaSor =\=Sor ->listahoz_fuz(VanIttFa,s,Tovabbi,Eredm); Eredm=Tovabbi)
    ;Eredm=Tovabbi)
    .


egy_fa_iranyai(Ss,Os,_,FaSor-FaOszlop,Fs,Eredm,w):-
    UjFaOszlop is FaOszlop-1,
    (   lehet_e_fa(Ss,Os,FaSor-UjFaOszlop) -> 
    van_e_fa(FaSor,UjFaOszlop,Fs,VanIttFa),
    (   UjFaOszlop =\= 0->listahoz_fuz(VanIttFa,w,[],Eredm);Eredm=[])
    ;Eredm=[])
    .
% listahoz fuz ha 0 egyebkent nem
% :- pred listahoz_fuz(int::in,irany::in,list(irany)::in,list(irany)::out).
listahoz_fuz(0,Eleje,Vege,Lista):-
    Lista=[Eleje|Vege].
listahoz_fuz(1,_,Vege,Lista):-
    Lista=Vege.

% :- pred volt_mar(list(int)::in,list(int)::out)
volt_mar([],[]).
volt_mar([_|T],VOLT_TOMB):-
    VOLT_TOMB=[0|Maradek],
    volt_mar(T,Maradek).

volt_mar(_,_,[],[]).

volt_mar(I,Iter,[_|T],VOLT_TOMB):-
    (   I=\=Iter -> VOLT_TOMB=[0|Maradek]; VOLT_TOMB=[1|Maradek]),
    Ujiter is Iter+1,
    volt_mar(I,Ujiter,T,Maradek).

% :- pred sator_szukites(list(int)::in,list(int)::in,list(int-int)::in,list(int-int)::in,list(int-int)::in,list(int-int)::out).
% rekurzivan sator szukit 
sator_szukites(Fs,ILs0,IranyKoord,ILs,EredmIK):-
    volt_mar(ILs0,VOLT),
    lista_hossz(ILs0,Hossz),
    egyesek_beirasa(Fs,ILs0,VOLT,Hossz,ILs0,IranyKoord,VOLT,Fs,0,Temp,UIK),
    (   member([],Temp)->fail;ILs=Temp,EredmIK=UIK),!.
    
% :- pred masodik_sator_szukites(list(int)::in,list(int)::in,list(int-int)::in,list(int-int)::in,list(int-int)::in,list(int-int)::out).
masodik_sator_szukites(Fs,ILs0,IranyKoord,ILs,EredmIK):-
    fals_input(1,1,ILs0,Fs,KeresettKoord,KeresettIrany,Kilepett),
    (   Kilepett=:=1 ->  fail;volt_mar(1,1,ILs0,VOLT)), 
    update_all_tree(KeresettKoord,KeresettIrany,IranyKoord,ILs0,VOLT,UIK,Temp),
    (   member([],Temp)->fail;ILs=Temp,EredmIK=UIK).


fals_input(_,_,_,_,[],_,1).
% :- pred fals_input(int::in,         % I
%                    int::in,         % Iter
%                    fák::in,         % Soruj
%                    iránylisták::in,         % Oszlopuj
%                    parcMutató::out,         % Eredmkoord
%                    irány::out, % Eredm
%                    int::out)        % Kilepe
% megadja, hogy a keresett fához tényleg egy irány tartozik-e illetve ezt a koordinátát,irányt visszaadja
fals_input(I,Iter,[H|T],[HF|TF],Eredmkoord,Eredm,Kilepe):-
    UjIter is Iter+1,
    (   I=:=Iter ->  lista_hossz(H,Hossz),(   Hossz=:=1 -> Eredm=H,Eredmkoord=HF,Kilepe=0 ;Kilepe=1);
    fals_input(I,UjIter,T,TF,Eredmkoord,Eredm,Kilepe)).

iranyok_koordinatai([],[],[]).
% :- pred iranyok_koordinatai(iránylisták::in,iránylisták::in,parcMutató::out).
iranyok_koordinatai([HF|TF],[HIL|TIL],Eredm):-
    Eredm=[EgyFa|Maradek],
    egy_fa_koordinatai(HF,HIL,EgyFa),
    iranyok_koordinatai(TF,TIL,Maradek).

egy_fa_koordinatai(_,[],[]).
% :- pred egy_fa_koordinatai(fák::in,iránylisták::in,parcMutató::out).
egy_fa_koordinatai(Sor-Oszlop,[e|TI],Eredm):-
    Oszlopuj is Oszlop +1,
    Eredm=[Sor-Oszlopuj|Maradek],
    egy_fa_koordinatai(Sor-Oszlop,TI,Maradek).
egy_fa_koordinatai(Sor-Oszlop,[n|TI],Eredm):-
    Soruj is Sor -1,
    Eredm=[Soruj-Oszlop|Maradek],
    egy_fa_koordinatai(Sor-Oszlop,TI,Maradek).
egy_fa_koordinatai(Sor-Oszlop,[s|TI],Eredm):-
    Soruj is Sor +1,
    Eredm=[Soruj-Oszlop|Maradek],
    egy_fa_koordinatai(Sor-Oszlop,TI,Maradek).
egy_fa_koordinatai(Sor-Oszlop,[w|TI],Eredm):-
    Oszlopuj is Oszlop -1,
    Eredm=[Sor-Oszlopuj|Maradek],
    egy_fa_koordinatai(Sor-Oszlop,TI,Maradek).
    
lista_hossz([],0).
% :- pred lista_hossz(list(int)::in,int::out).
lista_hossz([_|T],N) :- lista_hossz(T,N1), N is N1 + 1.

lista_kicserel(_,[],[],_,_).
% :- pred lista_kicserel(int::in,list(int)::in,list(int)::in,int::in,int::out).
lista_kicserel(I,[H|T],UjLista,Csere,N):-
    IUj is I+1,
    (   I=:=N ->  UjLista=[Csere|Maradek] 
    ;UjLista=[H|Maradek]),
    lista_kicserel(IUj,T,Maradek,Csere,N).

lista_csokkent(_,[],[],_).
lista_csokkent(I,[H|T],UjLista,N):-
    IUj is I+1,
    (   I=:=N -> 
    UjH is H-1,
    UjLista=[UjH|Maradek] 
    ;UjLista=[H|Maradek]),
    lista_csokkent(IUj,T,Maradek,N).

update_tree_iranyai(_,[],[],[],[]).
% :- pred update_tree_iranyai(int::in,iránylisták::in,iránylisták::in,iránylisták::in,iránylisták::out).
update_tree_iranyai(SOR-OSZLOP,[IRANYKSOR-IRANYKOSZLOP|TIK],[HI|TI],UJIRANY,UJIRANYKORD):-
    (   SOR=:=IRANYKSOR -> (   
                          OSZLOP=:=IRANYKOSZLOP ->    UJIRANYKORD=TIK,UJIRANY=TI
                          ;
                          UJIRANYKORD=[IRANYKSOR-IRANYKOSZLOP|MarIK],
                              UJIRANY=[HI|MarIr],
                          update_tree_iranyai(SOR-OSZLOP,TIK,TI,MarIr,MarIK))
    ;UJIRANYKORD=[IRANYKSOR-IRANYKOSZLOP|MarIK],
                              UJIRANY=[HI|MarIr],
                          update_tree_iranyai(SOR-OSZLOP,TIK,TI,MarIr,MarIK)).


update_all_tree(_,_,[],[],[],[],[]).
% :- pred update_all_tree(int::in,int::in,iránylisták::in,iránylisták::in,iránylisták::in,iránylisták::in,iránylisták::out).
update_all_tree(NM,Irany,[HIK|TIK],[HIR|TIR],[HV|TV],UJIK,UJIR):-
    UJIK=[EgyIK|MaradekIK],
    UJIR=[EgyIR|MaradekIR],
    update_tree(NM,HIK,HIR,HV,Irany,EgyIR,EgyIK),
    update_all_tree(NM,Irany,TIK,TIR,TV,MaradekIK,MaradekIR).
    

update_tree(_,[],[],_,_,[],[]).
update_tree(_,IK,IRANY,1,_,IRANY,IK).
    
% :- pred update_tree(int::in,iránylisták::in,iránylisták::in,int::in,irány::in,iránylisták::out,iránylisták::out).
update_tree(Sor-Oszlop,IK,IRANY,0,[e],UjIrany,UjIK):-
    Oszlopuj is Oszlop+1,
    update_all_9_mezo(Sor-Oszlopuj,IK,IRANY,UjIrany,UjIK).
update_tree(Sor-Oszlop,IK,IRANY,0,[w],UjIrany,UjIK):-
    Oszlopuj is Oszlop-1,
    update_all_9_mezo(Sor-Oszlopuj,IK,IRANY,UjIrany,UjIK).
update_tree(Sor-Oszlop,IK,IRANY,0,[n],UjIrany,UjIK):-
    Soruj is Sor-1,
    update_all_9_mezo(Soruj-Oszlop,IK,IRANY,UjIrany,UjIK).
update_tree(Sor-Oszlop,IK,IRANY,0,[s],UjIrany,UjIK):-
    Soruj is Sor+1,
    update_all_9_mezo(Soruj-Oszlop,IK,IRANY,UjIrany,UjIK).
    
% :- pred update_all_9_mezo(koordináta::in,iránylisták::in,iránylisták::in,iránylisták::out,iránylisták::out).
update_all_9_mezo(Sor1-Oszlop1,IK,IRANY,UjIrany,UjIK):-
    Sor0 is Sor1-1,
    Sor2 is Sor1+1,
    Oszlop0 is Oszlop1-1,
    Oszlop2 is Oszlop1+1,
    update_tree_iranyai(Sor0-Oszlop0,IK,IRANY,UjIrany1,UjIK1),
    update_tree_iranyai(Sor0-Oszlop1,UjIK1,UjIrany1,UjIrany2,UjIK2),
    update_tree_iranyai(Sor0-Oszlop2,UjIK2,UjIrany2,UjIrany3,UjIK3),
    update_tree_iranyai(Sor1-Oszlop0,UjIK3,UjIrany3,UjIrany4,UjIK4),
    update_tree_iranyai(Sor1-Oszlop1,UjIK4,UjIrany4,UjIrany5,UjIK5),
    update_tree_iranyai(Sor1-Oszlop2,UjIK5,UjIrany5,UjIrany6,UjIK6),
    update_tree_iranyai(Sor2-Oszlop0,UjIK6,UjIrany6,UjIrany7,UjIK7),
    update_tree_iranyai(Sor2-Oszlop1,UjIK7,UjIrany7,UjIrany8,UjIK8),
    update_tree_iranyai(Sor2-Oszlop2,UjIK8,UjIrany8,UjIrany,UjIK).
    
%iter 0tol induljon
% :- pred egyesek_beirasa(int::in,iránylisták::in,iránylisták::in,iránylisták::in,iránylisták::out,iránylisták::out,iránylisták::out).
egyesek_beirasa([FaH|FaT],[HIrany|TIrany],[HVolt|TVolt],I,Iranyok,IK,VOLT,Fak,Iter,Eredm,EredmIK):-
    lista_hossz(HIrany,Hossz),
    lista_hossz(TIrany,H2),
    (   H2>0 ->  (   member([],TIrany) ->   fail; true);true),
    UjIter is Iter+1,
    (   Hossz=:=1 -> 
      (HVolt=:=0 ->  lista_kicserel(0,VOLT,UJVOLT,1,Iter),
          update_all_tree(FaH,HIrany,IK,Iranyok,UJVOLT,UjIK,UjIR),
          egyesek_beirasa(Fak,UjIR,UJVOLT,I,UjIR,UjIK,UJVOLT,Fak,0,Eredm,EredmIK)
      ; 
          egyesek_beirasa(FaT,TIrany,TVolt,I,Iranyok,IK,VOLT,Fak,UjIter,Eredm,EredmIK)
      )
    ;egyesek_beirasa(FaT,TIrany,TVolt,I,Iranyok,IK,VOLT,Fak,UjIter,Eredm,EredmIK)
	)
.
    
egyesek_beirasa(_,[],_,I,Iranyok,IK,_,_,I,Eredm,EredmIK):-Eredm=Iranyok,EredmIK=IK.
    

% :- pred osszeg_szukites(iránylisták::in,iránylisták::in,int::in,int::out).
osszeg_szukites(sor(Sor,Db),IranyKoord, ILs0, ILs,IK):-
    biztos_esetleg_sor(Sor,IranyKoord,BiztosSzam,EsetlegSzam,BETomb),
    ( BiztosSzam+EsetlegSzam < Db ->  fail;
    	BiztosSzam =:= Db -> esetleges_kivetel_sor(Sor,IranyKoord,ILs0,BETomb,ILs,IK);
        BiztosSzam+EsetlegSzam =:= Db -> esetleges_biztos_berak_sor(Sor,IranyKoord,ILs0,BETomb,ILs,IK) ;
        BiztosSzam> Db -> fail;
    	ILs=ILs0,IK=IranyKoord
    ).
    

osszeg_szukites(oszl(Oszlop,Db),IranyKoord, ILs0, ILs,IK):-
    biztos_esetleg_oszlop(Oszlop,IranyKoord,BiztosSzam,EsetlegSzam,BETomb),
    ( BiztosSzam+EsetlegSzam < Db ->  fail;
        BiztosSzam =:= Db -> esetleges_kivetel_oszlop(Oszlop,IranyKoord,ILs0,BETomb,ILs,IK);
        BiztosSzam+EsetlegSzam =:= Db -> esetleges_biztos_berak_oszlop(Oszlop,IranyKoord,ILs0,BETomb,ILs,IK) ;
        BiztosSzam> Db -> fail;
    	ILs=ILs0,IK=IranyKoord
    ).

esetleges_kivetel_sor(_,[],[],[],[],[]).
% :- pred esetleges_kivetel_sor(int::in, list(int-int)::in, iránylisták::in, list(int)::in, iránylisták::out).
% kiveszi az esetleges iranyokat sor alapjan
esetleges_kivetel_sor(Sor,[HIK|TIK],[HIR|TIR],[HB|TB],Eredm,ErIk):-
    (   HB=:=1 -> egy_esetleg_kivesz_sor(Sor,HIK,HIR,UjIrany,UjIrk),Eredm=[UjIrany|UjEr],ErIk=[UjIrk|UjIk];
    Eredm=[HIR|UjEr],
    ErIk=[HIK|UjIk]
    ),
    esetleges_kivetel_sor(Sor,TIK,TIR,TB,UjEr,UjIk).

egy_esetleg_kivesz_sor(_,[],[],[],[]).
% :- pred egy_esetleg_kivesz_sor(int::in, int-int::in, iránylista::in, iránylista::out).
% egy iranylistabol kiveszi az esetleges iranyokat
egy_esetleg_kivesz_sor(Sor,[S-O|TIK],[HIR|TIR],ER,ErIk):-
    (   Sor=:=S ->  egy_esetleg_kivesz_sor(Sor,TIK,TIR,ER,ErIk);
    ER=[HIR|Maradek],
        ErIk=[S-O|UjIk],
    	egy_esetleg_kivesz_sor(Sor,TIK,TIR,Maradek,UjIk)
    ).

esetleges_kivetel_oszlop(_,[],[],[],[],[]).
% :- pred esetleges_kivetel_oszlop(int::in, list(int-int)::in, iránylisták::in, list(int)::in, iránylisták::out).
% kiveszi az esetleges iranyokat oszlop alapjan
esetleges_kivetel_oszlop(Oszlop,[HIK|TIK],[HIR|TIR],[HB|TB],Eredm,ErIk):-
    (   HB=:=1 -> egy_esetleg_kivesz_oszlop(Oszlop,HIK,HIR,UjIrany,UjIrk),Eredm=[UjIrany|UjEr],ErIk=[UjIrk|UjIk];
    Eredm=[HIR|UjEr],
    ErIk=[HIK|UjIk]
    ),
    esetleges_kivetel_oszlop(Oszlop,TIK,TIR,TB,UjEr,UjIk).

egy_esetleg_kivesz_oszlop(_,[],[],[],[]).
% :- pred egy_esetleg_kivesz_oszlop(int::in, int-int::in, iránylista::in, iránylista::out).
% egy iranylistabol kiveszi az esetleges iranyokat
egy_esetleg_kivesz_oszlop(Oszlop,[S-Osz|TIK],[HIR|TIR],ER,ErIk):-
    (   Oszlop=:=Osz ->  egy_esetleg_kivesz_oszlop(Oszlop,TIK,TIR,ER,ErIk);
    ER=[HIR|Maradek],
    ErIk=[S-Osz|UjIk],
    	egy_esetleg_kivesz_oszlop(Oszlop,TIK,TIR,Maradek,UjIk)
    ).

esetleges_biztos_berak_sor(_,[],[],[],[],[]).
% :- pred esetleges_biztos_berak_sor(int::in, list(int-int)::in, iránylisták::in, list(int)::in, iránylisták::out).
% berakja az esetleges es biztos iranyokat sor alapjan
esetleges_biztos_berak_sor(Sor,[HIK|TIK],[HIR|TIR],[HB|TB],Eredm,ErIk):-
    (   HB=\=2 -> egy_eset_bizt_berak_sor(Sor,HIK,HIR,UjIrany,UjIrk),Eredm=[UjIrany|UjEr],ErIk=[UjIrk|UjIk];
    (HB=:=0 ->  egy_eset_bizt_berak_sor(Sor,HIK,HIR,UjIrany,UjIrk),Eredm=[UjIrany|UjEr],ErIk=[UjIrk|UjIk];
    Eredm=[HIR|UjEr],
    ErIk=[HIK|UjIk]
    )
    ),
    esetleges_biztos_berak_sor(Sor,TIK,TIR,TB,UjEr,UjIk).

egy_eset_bizt_berak_sor(_,[],[],[],[]).
% :- pred egy_eset_bizt_berak_sor(int::in, int-int::in, iránylista::in, iránylista::out).
egy_eset_bizt_berak_sor(Sor,[S-O|TIK],[HIR|TIR],ER,ErIk):-
    (   Sor=:=S -> 
    ER=[HIR|Mar],
	ErIk=[S-O|UjEr],
    egy_eset_bizt_berak_sor(Sor,TIK,TIR,Mar,UjEr);
    egy_eset_bizt_berak_sor(Sor,TIK,TIR,ER,ErIk)
    ).

esetleges_biztos_berak_oszlop(_,[],[],[],[],[]).
% :- pred esetleges_biztos_berak_oszlop(int::in, list(int-int)::in, iránylisták::in, list(int)::in, iránylisták::out).
esetleges_biztos_berak_oszlop(Oszlop,[HIK|TIK],[HIR|TIR],[HB|TB],Eredm,ErIk):-
    (   HB=:=1 -> egy_eset_bizt_berak_oszlop(Oszlop,HIK,HIR,UjIrany,UjIrk),Eredm=[UjIrany|UjEr],ErIk=[UjIrk|UjIk];
    (HB=:=0 ->  egy_eset_bizt_berak_oszlop(Oszlop,HIK,HIR,UjIrany,UjIrk),Eredm=[UjIrany|UjEr],ErIk=[UjIrk|UjIk];
    Eredm=[HIR|UjEr],
    ErIk=[HIK|UjIk]
    )
    ),
    esetleges_biztos_berak_oszlop(Oszlop,TIK,TIR,TB,UjEr,UjIk).

egy_eset_bizt_berak_oszlop(_,[],[],[],[]).
% :- pred egy_eset_bizt_berak_oszlop(int::in, int-int::in, iránylista::in, iránylista::out).
egy_eset_bizt_berak_oszlop(Oszlop,[S-Osz|TIK],[HIR|TIR],ER,ErIk):-
    (   Oszlop=:=Osz -> 
    ER=[HIR|Mar],
        ErIk=[S-Osz|UjIk],
    egy_eset_bizt_berak_oszlop(Oszlop,TIK,TIR,Mar,UjIk);
    	egy_eset_bizt_berak_oszlop(Oszlop,TIK,TIR,ER,ErIk)
    ).


biztos_esetleg_oszlop(_,[],0,0,[]).  
% :- pred biztos_esetleg_oszlop(int::in, list(int-int)::in, int::out, int::out, list(int)::out).
biztos_esetleg_oszlop(Oszlop,[HIK|TIK],BiztosSzam,EsetlegSzam,BETomb):-
    egy_fa_biztos_esetleg_oszlop(Oszlop,HIK,BEr,EEr,0,0),
    (   BEr=:=1 ->   BETomb=[0|UjTomb] ; (   EEr=:=1 ->   BETomb=[1|UjTomb]; BETomb=[2|UjTomb])),
    biztos_esetleg_oszlop(Oszlop,TIK,BiztosUj,EsetlegUj,UjTomb),
    BiztosSzam is BiztosUj+BEr,
    EsetlegSzam is EsetlegUj+EEr.
egy_fa_biztos_esetleg_oszlop(_,[],BEr,EEr,VoltBiztos,VoltMas):-
    (   VoltMas=:=1 ->  BEr=0,(VoltBiztos=:=1->  EEr=1,BEr=0;EEr=0); ( VoltBiztos=:=1 ->  EEr=0,BEr=1  ) ).

% :- pred egy_fa_biztos_esetleg_oszlop(int::in, int-int::in, int::out, int::out, int::in, int::in).
egy_fa_biztos_esetleg_oszlop(Oszlop,[_-OszlopM|T],BEr,EEr,VoltBiztos,VoltMas):-
    (   Oszlop=:=OszlopM ->  egy_fa_biztos_esetleg_oszlop(Oszlop,T,BEr,EEr,1,VoltMas);
    egy_fa_biztos_esetleg_oszlop(Oszlop,T,BEr,EEr,VoltBiztos,1)).

biztos_esetleg_sor(_,[],0,0,[]).  
% :- pred biztos_esetleg_sor(int::in, list(int-int)::in, int::out, int::out, list(int)::out).
biztos_esetleg_sor(Sor,[HIK|TIK],BiztosSzam,EsetlegSzam,BETomb):-
    egy_fa_biztos_esetleg_sor(Sor,HIK,BEr,EEr,0,0),
    (   BEr=:=1 ->   BETomb=[0|UjTomb] ; (   EEr=:=1 ->   BETomb=[1|UjTomb]; BETomb=[2|UjTomb])),
    biztos_esetleg_sor(Sor,TIK,BiztosUj,EsetlegUj,UjTomb),
    BiztosSzam is BiztosUj+BEr,
    EsetlegSzam is EsetlegUj+EEr.
egy_fa_biztos_esetleg_sor(_,[],BEr,EEr,VoltBiztos,VoltMas):-
    (   VoltMas=:=1 ->  BEr=0,(VoltBiztos=:=1->  EEr=1,BEr=0;EEr=0); ( VoltBiztos=:=1 ->  EEr=0,BEr=1  ) ).

% :- pred egy_fa_biztos_esetleg_sor(int::in, int-int::in, int::out, int::out, int::in, int::in).
egy_fa_biztos_esetleg_sor(Sor,[SorM-_|T],BEr,EEr,VoltBiztos,VoltMas):-
    (   Sor=:=SorM ->  egy_fa_biztos_esetleg_sor(Sor,T,BEr,EEr,1,VoltMas);
    egy_fa_biztos_esetleg_sor(Sor,T,BEr,EEr,VoltBiztos,1)).
