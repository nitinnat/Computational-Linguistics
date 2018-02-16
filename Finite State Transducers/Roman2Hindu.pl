q0([],[]).
q0([i],[1|L]) :- q1([],L).
q0([i|T],L) :- T \== [], q1(T,L).
q0([v],[5|L]) :- q4([],L).
q0([v|T],L) :- T \== [], q4(T,L).
q0([x],[1,0|L]) :- q8([],L).
q0([x|T],L) :- T \== [], q8(T,L).
q8([],[]).
q8([x],[2,0|L]) :- q9([],L).
q8([v],[1,5]) :- q4([],L).
q8([v|T],[1|L]) :- T \== [], q4(T,L).
q8([i],[1,1|L]) :- q1([],L).
q8([i|T],[1|L]) :- T \== [], q1(T,L).

q9([],[]).



q1([],[]).
q1([i],[2|L]) :- q2([],L).
q1([i|T],L) :- T \== [], q2(T,L).
q1([v],[4|L]) :- q9([],L).
q1([x],[9|L]) :- q9([],L).


q2([],[]).
q2([i],[3|L]) :- q3([],L).

q3([],[]).


q4([],[]).
q4([i],[6|L]) :- q5([],L).
q4([i|T],L) :- T \== [], q5(T,L).

q5([],[]).
q5([i],[7|L]) :- q6([],L).
q5([i|T],L) :- T \== [], q6(T,L).

q6([],[]).
q6([i],[8|L]) :- q7([],L).

q7([],[]).















