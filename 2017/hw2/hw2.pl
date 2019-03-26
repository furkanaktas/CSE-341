%---------------%
%---  PART 3 ---%
%---------------%

when(a,10).
when(b,12).
when(c,11).
when(d,11).
when(e,17).
when(f,10).
when(g,9).

where(a,101).
where(b,104).
where(c,102).
where(d,103).
where(e,103).
where(f,101).
where(g,104).

enrol(1,a).
enrol(1,b).
enrol(2,a).
enrol(3,b).
enrol(4,c).
enrol(5,d).
enrol(6,d).
enrol(6,a).
enrol(7,a).
enrol(7,b).
enrol(7,g).
enrol(8,d).
enrol(8,f).
enrol(9,a).
enrol(9,g).
enrol(10,b).
enrol(10,e).
enrol(11,c).
enrol(11,f).
enrol(11,g).


% 3.1
schedule(S,P,T):- 
    enrol(S,X),   /* hangi dersi aldığı*/
    where(X,P),    /*dersin sınıfı*/
    when(X,T).      /*dersin zamanı*/

% 3.2
usage(P,T):- 
    where(X,P),     /* girilen dersin sınıfı*/
    when(X,T).      /* zamanı */


% 3.3
confRoom(X,Y):-  
    where(X,X1),
    where(Y,Y1),
    X\==Y,          /* aynı ders değilse */
    X1==Y1.         /* girilen derslerin, sınıfları çakışıyorsa*/
confTime(X,Y):-
    when(X,X1),
    when(Y,Y1),
    X\==Y,          /* aynı ders değilse */
    X1 == Y1.       /* girilen derslerin, saatleri çakışıyorsa*/
conflict(X,Y):-  
    not(
        (not(confRoom(X,Y)),    /* girilen derslerin, sınıfları YADA saatleri çakışıyorsa*/
        not(confTime(X,Y)))).

% 3.4
meet(X,Y):- 
    enrol(X,C1),
    where(C1,P1),
    enrol(Y,C2),
    where(C2,P2),
    C1==C2,P1==P2,      /*aynı dersleri alıyorsa ve sınıfları aynıysa */
    X\==Y,      /*Aynı kişi değilse*/
    !.

%---------------%
%---  PART 4 ---%
%---------------%

/*union*/
union([A|B], C, U) :- 
    member(A,C),        /*A 2. listede varsa */
    !, 
    union(B,C,U).       /*1. listenin tail'i (B) ile, union değişmeden devam*/
union([A|B], C, [A|U]) :- /*önce 2. listede olmayan elemanlar sonra, 2.'den  kalanlar eklenir*/
    union(B,C,U).
union([],Z,Z).   /*List 1 boşsa , union = List2*/


/*intersect */
intersect([], _, []).   /* listelerden herhangi biri boşsa, intersect = []*/
intersect([A|B], C, I) :-
    member(A, C),       /*A 2. listede varsa*/
    !,
    I = [A|Tail],        /* A I'ya eklenir*/
    intersect(B, C, Tail).  /*1. listenin tail ile devam*/
intersect([_|B], C, I) :-  
    intersect(B, C, I).      /*1. listeden gelen eleman, 2. de yoksa direk 1.nin tail'le devam*/



/*flatten*/
flatten([], []) :- !.       /* liste boşsa, flatten = []*/
flatten([L|Ls], F) :-
    !,
    flatten(L, New1),   /* nested yapılar için */
    flatten(Ls, New2),  /* tail yollanır, tüm elemanları gezmek için */
    append(New1, New2, F).  /* sonuç lar eklenir*/
flatten(L, [L]).        /*atom eleman geldiğinde kendisi */







