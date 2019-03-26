%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PART 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


flight(trabzon,istanbul,3).
flight(trabzon,ankara,6).

flight(istanbul,trabzon,3).
flight(istanbul,ankara,2).
flight(istanbul,izmir,3).

flight(izmir,istanbul,3).
flight(izmir,ankara,6).
flight(izmir,antalya,1).

flight(ankara,trabzon,6).
flight(ankara,kars,3).
flight(ankara,diyarbakir,8).
flight(ankara,izmir,6).
flight(ankara,istanbul,2).

flight(kars,ankara,3).
flight(kars,gaziantep,3).

flight(gaziantep,kars,3).

flight(diyarbakir,ankara,8).
flight(diyarbakir,antalya,5).

flight(antalya,diyarbakir,5).
flight(antalya,izmir,1).
flight(antalya,erzurum,2).

flight(erzurum,antalya,2).
flight(erzurum,edirne,5).

flight(edirne,erzurum,5).




% VISITED ve ROUTE listesi vardır
% gidilen şehirler VISITED a eklenir, ROUTE yolu tutar 
% hedef şehre ulaşıldığında, ROUTE ile VISITED aynı olur ve orda tamamlanır (ilk move (üstteki) )
%
% hedef şehre predicate ile gidildiğinden(Z), hedef için farklı rotalara recurison ile denenmiş olur.

route(X,Y,C) :- 
	move(X,Y,_,[X],C).

move(X,Y,[Y|ROUTE],ROUTE,C) :- 
	 flight(X,Y,C).

move(X,Y,ROUTE,VISITED,C) :- 
	flight(X,Z,A), \+ member(Z, VISITED), move(Z,Y,ROUTE,[Z|VISITED],B), C is A+B.
