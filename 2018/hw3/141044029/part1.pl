																							   	
%  																					           %  
% Room       - id capacity saatler özellikler(projeksiyon, akıllı tahta)                       %         
% Course     - id capacity saatler özellikler  instructor  (özelliğe göre room ile eşleştir)   %   
% student    - id list of courses															   %    
% instructor - id courses özellikler                                                      	   %   
%  																					           %  
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  id  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% room 
:- dynamic id/3.

id(room1, z06, room).
id(room2, z11, room).

% course

id(pl   , cse341, course).
id(soft , cse343, course).
id(org  , cse331, course).
id(algho, cse321, course).

% instructor

id(yakup , genc, instructor).
id(cengiz, turker, instructor).
id(alp   , bayrakci, instructor).
id(didem , gozupek, instructor).

% student

id(student1, 1, student).
id(student2, 2, student).
id(student3, 3, student).
id(student4, 4, student).
id(student5, 5, student).
id(student6, 6, student).
id(student7, 7, student).
id(student8, 8, student).
id(student9, 9, student).
id(student10, 10, student).
id(student11, 11, student).
id(student12, 12, student).
id(student13, 13, student).
id(student14, 14, student).
id(student15, 15, student).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  capacity  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% room
:- dynamic capacity/2.

capacity(z06, 10).
capacity(z11, 10).

% course

capacity(cse341, 10).
capacity(cse343, 6).
capacity(cse331, 5).
capacity(cse321, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  when  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% room
:- dynamic when/3.

when(z06, 8, cse341).
when(z06, 9, cse341).
when(z06, 10, cse341).
when(z06, 11, cse341).
when(z06, 13, cse331).
when(z06, 14, cse331).
when(z06, 15, cse331).

when(z11, 8, cse343).
when(z11, 9, cse343).
when(z11, 10, cse343).
when(z11, 11, cse343).
when(z11, 14, cse321).
when(z11, 15, cse321).
when(z11, 16, cse321).

% course

when(cse341, 8, z06).
when(cse341, 9, z06).
when(cse341, 10, z06).
when(cse341, 11, z06).
when(cse331, 13, z06).
when(cse331, 14, z06).
when(cse331, 15, z06).

when(cse343, 8, z11).
when(cse343, 9, z11).
when(cse343, 10, z11).
when(cse343, 11, z11).
when(cse321, 14, z11).
when(cse321, 15, z11).
when(cse321, 16, z11).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  hour usage  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic hour/2.
hour(cse341, 4).
hour(cse343, 3).
hour(cse331, 3).
hour(cse321, 4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  equipment  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% room
:- dynamic equipment/2.

equipment(z06, [hcapped, projector]).
equipment(z11, [hcapped, smartboard]).

% course

equipment(cse341, []).
equipment(cse343, []).
equipment(cse331, []).
equipment(cse321, []).

% instructor

equipment(genc, [projector]).
equipment(turker, [smartboard]).
equipment(bayrakci, []).
equipment(gozupek, [smartboard]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  responsibility  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% room
:- dynamic responsibility/2.

responsibility(z06, [cse341, cse331]).
responsibility(z11, [cse343, cse321]).

% course

responsibility(cse341, [1, 2, 3, 4, 6, 7, 8, 9, 10, 11]).
responsibility(cse343, [1, 2, 5, 6, 7, 12, 13, 14, 15]).
responsibility(cse331, [1, 3, 5, 6, 8]).
responsibility(cse321, [10, 11, 12, 13, 14, 15]).

% instructor

responsibility(genc, [cse341]).
responsibility(turker, [cse343]).
responsibility(bayrakci, [cse331]).
responsibility(gozupek, [cse321]).

% student

responsibility(1, [cse341, cse343, cse331]).
responsibility(2, [cse341, cse343]).
responsibility(3, [cse341, cse331]).
responsibility(4, [cse341]).
responsibility(5, [cse343, cse331]).
responsibility(6, [cse341, cse343, cse331]).
responsibility(7, [cse341, cse343]).
responsibility(8, [cse341, cse331]).
responsibility(9, [cse341]).
responsibility(10, [cse341, cse321]).
responsibility(11, [cse341, cse321]).
responsibility(12, [cse343, cse321]).
responsibility(13, [cse343, cse321]).
responsibility(14, [cse343, cse321]).
responsibility(15, [cse343, cse321]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  hancapped  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% student
:- dynamic hancapped/2. 

hancapped(1,  no).
hancapped(2,  no).
hancapped(3,  no).
hancapped(4,  no).
hancapped(5,  no).
hancapped(6,  yes).
hancapped(7,  no).
hancapped(8,  yes).
hancapped(9,  no).
hancapped(10, no).
hancapped(11, no).
hancapped(12, no).
hancapped(13, no).
hancapped(14, no).
hancapped(15, yes).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    QUERIES    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    HELPERS    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



time_conflict(ID1, ID2) :-
	ID1 \== ID2, id(_, ID1, course), id(_, ID2, course) , when(ID1, T1, _), when(ID2, T2, _), T1 == T2.  % student veya instructor için aynı saat te aldığı ders varsa

course_conflict_helper([], _).
course_conflict_helper([A| REST], COURSE_ID) :- 
	\+ time_conflict(A, COURSE_ID), course_conflict_helper(REST, COURSE_ID).

course_conflict(LIST, COURSE_ID) :- \+ course_conflict_helper(LIST, COURSE_ID).



contains_all([], _).
contains_all([A|Rest], LIST2) :-   	% tüm hepsini içeriyor mu
	member(A, LIST2), contains_all(Rest, LIST2).

contains_hcapped([]).
contains_hcapped([A|Rest]) :-  	% hcapped var mı yok mu
	hancapped(A, no), contains_hcapped(Rest).


incr(X, X1) :-		% sayı += 1
    X1 is X+1.	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    CONFLICT    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

conflict(ID1, ID2) :- 
	ID1 \== ID2  , id(_, ID1, course), id(_, ID2, course) , when(ID1, T1, R1), when(ID2, T2, R2), R1 == R2 , T1 == T2 ; 	 					% farklı course, aynı saat, aynı sınıf
	ID1 \== ID2  , id(_, ID1, room)  , id(_, ID2, room)   , when(ID1, T1, C1), when(ID2, T2, C2), C1 == C2 , T1 == T2 ; 	 					% farklı oda, aynı saat, aynı derslere verildiyse
	(id(_, ID1, student) ; id(_, ID1, instructor))  , id(_, ID2, course), responsibility(ID1, CourseList), course_conflict(CourseList, ID2).  	% aynı zamanda, 2 farklı ders alan/veren  varsa


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    ASSIGN   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    STUDENTS    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign(STDNT_ID, COURSE_ID) :-
	id(_, STDNT_ID, student)  , id(_, COURSE_ID, course),
	responsibility(STDNT_ID, CourseList)    , \+ member(COURSE_ID,CourseList), \+ conflict(STDNT_ID, COURSE_ID). 			% zaten dersi almıyorsa ve aldığı derslerle çakışmıyorsa (zaman)
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    INSTRUCTOR    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign(INSTRCTOR_ID, COURSE_ID) :-
	id(_, INSTRCTOR_ID, instructor), id(_, COURSE_ID, course), equipment(INSTRCTOR_ID, EqList1), equipment(COURSE_ID, EqList2), contains_all(EqList1, EqList2),   % instructor ın tüm ekipman ihtiyacına, ders sahipse
	responsibility(INSTRCTOR_ID, CourseList)    , \+ member(COURSE_ID,CourseList), \+ conflict(INSTRCTOR_ID, COURSE_ID), 								  		  % zaten dersi vermiyorsa ve verdiği derslerle çakışmıyorsa (zaman)
	responsibility(CLASS, RCourseList), member(COURSE_ID, RCourseList), equipment(CLASS, EqList3), contains_all(EqList1, EqList3). 								  % instructor tüm ihtiyaçları, dersin verildiği sınıfta varsa 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    ROOM    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assign(ROOM_ID, COURSE_ID) :-
	id(_, ROOM_ID, room), equipment(ROOM_ID, EqList1), 
	id(_, COURSE_ID, course), equipment(COURSE_ID, EqList2),
	contains_all(EqList2, EqList1),  																											% dersin tüm ekipman ihtiyacına oda sahipse
	capacity(ROOM_ID, RoomCap), capacity(COURSE_ID, CourseCap), CourseCap =< RoomCap,   														% ders kapasitesi sınıf tan az yada eşit olmali
	responsibility(COURSE_ID, StudentList), ( \+ contains_hcapped(StudentList), member(hcapped, EqList1); contains_hcapped(StudentList) ). 		% dersi alan hcapped varsa, oda nın hcapped özelliği olmalı



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    ENROLL   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    STUDENTS    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enroll(STDNT_ID, COURSE_ID) :-
	id(_, STDNT_ID, student),id(_, COURSE_ID, course), 
	assign(STDNT_ID, COURSE_ID), 
	retract(responsibility(STDNT_ID, CourseList)), assert(responsibility(STDNT_ID, [COURSE_ID | CourseList])),   											% update responsibility list of student 											 	
	responsibility(COURSE_ID, StudentList), retract(responsibility(COURSE_ID, StudentList)), assert(responsibility(COURSE_ID, [STDNT_ID | StudentList])).   % update responsibility list of course


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    INSTRUCTOR    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enroll(INSTRCTOR_ID, COURSE_ID) :-
	id(_, INSTRCTOR_ID, instructor),id(_, COURSE_ID, course), 
	assign(INSTRCTOR_ID, COURSE_ID),
	retract(responsibility(INSTRCTOR_ID, CourseList)), assert(responsibility(INSTRCTOR_ID, [COURSE_ID | CourseList])).   	% update responsibility list of instructor 											 	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    ROOM    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enroll_room(ROOM_ID, COURSE_ID, TIME) :-
	TIME >= 8, TIME < 17,					 	% 8 - 17 arasında ise
	\+ when(ROOM_ID, TIME, _), 					% verilen zamanda oda dolu değilse
	id(_, ROOM_ID, room),id(_, COURSE_ID, course), 
	assign(ROOM_ID, COURSE_ID),
	assert( when(ROOM_ID, TIME, COURSE_ID)),
	assert( when(COURSE_ID, TIME, ROOM_ID)),
	hour(COURSE_ID, HOUR), retract(hour(COURSE_ID, HOUR)), incr(HOUR, N_HOUR) ,assert(hour(COURSE_ID, N_HOUR)),   % ders saat bilgisi güncellendi
	(responsibility(ROOM_ID, CourseList), \+ member(COURSE_ID, CourseList), retract(responsibility(ROOM_ID, CourseList)), assert(responsibility(ROOM_ID, [COURSE_ID | CourseList])) ; true).   	% update responsibility list of room if course doesnt exit in list.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

add_new_student(NAME, ID, HEALTH) :-
	\+ id(_, ID, student), 			% aynı id li student olmamalı
	assert( id(NAME, ID, student)),
	assert( hancapped(ID, HEALTH)),
	assert( responsibility(ID, [])).


add_new_course(NAME, ID, CAPACITY, EQUIPMENT) :-
	\+ id(_, ID, course),  			% aynı id li course olmamalı  
	assert( id(NAME, ID, course)),  
	assert( capacity(ID, CAPACITY)),
	assert( equipment(ID, EQUIPMENT)),
	assert( responsibility(ID, [])),
	assert( hour(ID, 0)).
	
	
add_new_room(NAME, ID, CAPACITY, EQUIPMENT) :-
	\+ id(_, ID, room),  			% aynı id li room olmamalı  
	assert( id(NAME, ID, room)),  
	assert( capacity(ID, CAPACITY)),
	assert( equipment(ID, EQUIPMENT)),
	assert( responsibility(ID, [])).


add_new_instructor(NAME, ID, EQUIPMENT) :-
	\+ id(_, ID, instructor), 			% aynı id li instructor olmamalı
	assert( id(NAME, ID, instructor)),
	assert( equipment(ID, EQUIPMENT)),
	assert( responsibility(ID, [])).




% add_new_course(czx, bil101, 10, []).
% add_new_room(czx, z23, 10, []).
% enroll(6, bil101).
% assign(S, bil101).