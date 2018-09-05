%Authors
%50246850 - Nitin Nataraj
%50249002 - Sunil Umashankar
% ===========================================================
% Main loop:
% 1. Repeat "input-response" cycle until input starts with "bye"
%    Each "input-response" cycle consists of:
%		1.1 Reading an input string and convert it to a tokenized list
%		1.2 Processing tokenized list
% ===========================================================

chat:-
 repeat,
   readinput(Input),
   process(Input),
  (Input = [bye| _] ),!.


% ===========================================================
% Read input:
% 1. Read char string from keyboard.
% 2. Convert char string to atom char list.
% 3. Convert char list to lower case.
% 4. Tokenize (based on spaces).
% ===========================================================

readinput(TokenList):-
   read_line_to_codes(user_input,InputString),
   string_to_atom(InputString,CharList),
   string_lower(CharList,LoweredCharList),
   tokenize_atom(LoweredCharList,TokenList).


% ===========================================================
%  Process tokenized input
% 1. Parse morphology and syntax, to obtain semantic representation
% 2. Evaluate input in the model
% If input starts with "bye" terminate.
% ===========================================================

process(Input):-
	parse(Input,SemanticRepresentation),
	modelchecker(SemanticRepresentation,Evaluation),
	respond(Evaluation),!,
	nl,nl.

process([bye|_]):-
   write('> bye!').


% ===========================================================
%  Parse:
% 1. Morphologically parse each token and tag it.
% 2. Add semantic representation to each tagged token
% 3. Obtain FOL representation for input sentence
% ===========================================================

%parse(Input, SemanticRepresentation):-
parse(String,S) :-
 leftcorner_recognize(S,String,[]).
 %numbervars(S,0,_),
 %write(S).


leftcorner_recognize(Cat,[Word|StringIn],StringOut) :-
 stem(Word,Stemmed), %Stem the word
 lex(WCat,Stemmed),
 complete(Cat,WCat,StringIn,StringOut).

complete(Cat,Cat,String,String).
complete(Cat,SubCat,StringIn,StringOut) :-
 rule(LHS,[SubCat|Cats]),
 matches(Cats,StringIn,String1),
 complete(Cat,LHS,String1,StringOut).

matches([],String,String).

matches([Cat|Cats],StringIn,StringOut) :-
 leftcorner_recognize(Cat,StringIn,String1),
 matches(Cats,String1,StringOut).



% ===========================================================
% Grammar
% 1. List of lemmas
% 2. Lexical items
% 3. Phrasal rules
% ===========================================================

% --------------------------------------------------------------------
% Lemmas are uninflected, except for irregular inflection
% lemma(+Lemma,+Category)
% --------------------------------------------------------------------
lemma(dog,n).
lemma(cat,n).
lemma(chase,tv).

%Determiners
lemma(a,dtexists).
lemma(an,dtexists).
lemma(each,dtforall).
lemma(all,dtforall).
lemma(every,dtforall).
lemma(the,dt).
lemma(some,dtexists).

lemma(one,number).
lemma(two,number).
lemma(three,number).
lemma(four,number).
lemma(five,number).
lemma(six,number).
lemma(seven,number).
lemma(eight,number).
lemma(nine,number).
lemma(ten,number).
%Proper nouns
lemma(mia,pn).
lemma(tom,pn).
lemma(sue,pn).
lemma(sam,pn).

%Nouns
lemma(box,n).
lemma(egg,n).
lemma(ham,n).
lemma(chicken,n).
lemma(beef,n).
lemma(bread,n).
lemma(freezer,n).
lemma(soup,n).
lemma(chicken,n).
lemma(can,n).
lemma(milk,n).
lemma(pork,n).
lemma(bowl,n).
lemma(milk,n).
%Adjectives
lemma(white,adj).
lemma(red,adj).
lemma(blue,adj).
lemma(yellow,adj).
lemma(green,adj).
lemma(orange,adj).

%Transitive verbs
lemma(ate,tv).
lemma(eat,tv).
lemma(like,tv).
lemma(drink,tv).
lemma(drank,tv).
lemma(drunk,tv).
lemma(drinking,tv).
lemma(contain,tv).
lemma(has,tv).
lemma(have,tv).
lemma(saw,tv).
lemma(see,tv).
lemma(belong,tv).

%Ditransitive verbs
lemma(put,dtv).
lemma(took,dtv).
lemma(take,dtv).
%Intransitive verbs
lemma(expire,iv).


%Prepositions
lemma(in,p).
lemma(inside,p).
lemma(under,p).
lemma(near,p).
lemma(on,p).

%These are needed for the ditransitive verb
lemma(in,vacpp).
lemma(inside,vacpp).
lemma(under,vacpp).
lemma(near,vacpp).
lemma(on,vacpp).

%Be
lemma(is,be).
lemma(are,be).
lemma(was,be).

%Auxiliary words
lemma(did,aux).
lemma(will,aux).
lemma(does,aux).


%There
lemma(there,there).

%Relative
lemma(that,rel).

%Vacuous preposition
lemma(to,vacp).

%Questions
lemma(who,who).
lemma(which,which).
lemma(what,what).
lemma(where,where).
%Adverb
lemma(not,not).
%! %%%%%%%%%%%%%%
lemma(passenger,n).
lemma(on,p).
lemma(bus,n).
lemma(sneeze,iv).


% --------------------------------------------------------------------
% Constructing lexical items:
% word = lemma + suffix (for "suffix" of size 0 or bigger)
% --------------------------------------------------------------------


%Proper noun - checked
lex(pn((Lemma^X)^X),Lemma):-
 lemma(Lemma,pn).

%Noun - checked
lex(n(X^P),Lemma):-
	lemma(Lemma,n),
	P=.. [Lemma,X].

%Prepositions - checked
lex(p((Y^R)^Q^(X^P)^and(P,Q)),Lemma):-
 lemma(Lemma,p),
 R =.. [Lemma,X,Y].

%Number
lex(number((X^P)^(X^Q)^R),Lemma):-
 lemma(Lemma,number),
 R =.. [Lemma,X,(and(P,Q))].


%Universal quantifier
lex(dt((X^P)^(X^Q)^forall(X,imp(P,Q))),Lemma):-
		lemma(Lemma,dtforall).

%Existential quantifier
lex(dt((X^P)^(X^Q)^exists(X,and(P,Q))),Lemma):-
 lemma(Lemma,dtexists).


%The
lex(dt((X^P)^(X^Q)^the(X,(and(P,Q)))),Lemma):-
 lemma(Lemma,dt).

%Adjective
lex(adj((X^P)^X^and(P,Q)),Lemma):-
 lemma(Lemma,adj),
 Q=.. [Lemma,X].

%Intransitive verb
lex(iv(X^P,[]),Lemma):-
 lemma(Lemma,iv),
 P=.. [Lemma,X].


%Transitive verb
lex(tv(X^Y^P,[]),Lemma):-
 lemma(Lemma,tv),
 P=.. [Lemma,X,Y].



%Auxiliary word
lex(aux,Lemma):-
 lemma(Lemma,aux).

%There and of
lex(there,there).
lex(of,of).
lex(not,not).
%Be
lex(be,Lemma):-
 lemma(Lemma,be).

%Relative
lex(rel,Lemma):-
 lemma(Lemma,rel).

%Vacuous preposition
lex(vacp,Lemma):-
 lemma(Lemma,vacp).


%Vacuous true preposition
%Needed to handle dtvs
lex(vacpp,Lemma):-
 lemma(Lemma,vacpp).


%Questions
lex(whpr((X^P)^q(X,and(person(X),P))),Lemma):-
    lemma(Lemma,who).
lex(whpr((X^P)^q(X,and(thing(X),P))),Lemma):-
    lemma(Lemma,what).
lex(whpr((X^P)^q(X,and(thing(X),P))),Lemma):-
    lemma(Lemma,which).
lex(whpr((X^P)^q(X,and(location(X),P))),Lemma):-
    lemma(Lemma,where).

%Ditransitive verb put
%X put Y on Z for example. Z
lex(dtv(X^Y^Z^W),Lemma):-
 lemma(Lemma,dtv),
 W =.. [Lemma,X,Y,Z].


% ...

% --------------------------------------------------------------------
% Suffix types
% --------------------------------------------------------------------

%-------------------------------------------------------------------
%Wordnet
%-------------------------------------------------------------------

is_parent_of(meat,ham).
is_parent_of(meat,chicken).
is_parent_of(meat,beef).
is_parent_of(meat,salmon).
is_parent_of(meat,fish).

is_ancestor_of(X,Y) :- is_parent_of(X,Y).
is_ancestor_of(X,Y) :- is_parent_of(X,Z), is_ancestor_of(Z,Y).


stem(Word,Lemma):-
 atom_concat(Lemma,_,Word),
 lemma(Lemma,tv),!.
stem(Word,Lemma):-
 atom_concat(Lemma,_,Word),
 lemma(Lemma,iv),!.
stem(Word,Lemma):-
 atom_concat(Lemma,_,Word),
 lemma(Lemma,n),!.

%If the word exists at all
stem(Word,Word):-
 lemma(Word,_),!.
%If the word does not exist in the lemma,return children
stem(Word,X):-
 atom_concat(Lemma,_,Word),
 is_parent_of(Lemma,X),!,is_ancestor_of(Lemma,X).

%If the word doesn't exist, check for its children in the wordnet

% --------------------------------------------------------------------
% Phrasal rules
% rule(+LHS,+ListOfRHS)
% --------------------------------------------------------------------

%in -> contain
%rule(tv(A^C^contain(D,B),[]),[np(A^B),vacpp,in,np(C^D)]).
%VP -> DTV,NP,NP
rule(vp(P^B, []), [dtv(P^X^Y^C), np((X^A)^B),vacpp, np((Y^C)^A)]).
%Negation
rule(vp(X^not(Y),[]),[not,vp(X^Y,[])]).

%The box is in the box
%S -> NP BE PP
rule(s(Z,[]),[np(X^Y),be,pp((X^Y)^Z)]).

%What is in the box
%Z -> WHPR YNQ
rule(Z, [whpr((_^Y)^Z) , ynq(Y)]).   %%
%Z -> WHPR BE PP
rule(Z, [whpr((B^C)^Z),be,pp((_^B)^C)]).
%S -> NP VP
rule(s(Y,WH),[np(X^Y),vp(X,WH)]).
%NP -> DT N
rule(np(C),[dt(A^C),n(A)]). %determiner + noun
%Numbers
rule(np(C),[number(A^C),n(A)]). %determiner + noun
%VP -> TV NP
rule(vp(X^K,[]),[tv(X^Y,[]),np(Y^K)]).
%N -> ADJ N
rule(n(Y),[adj(X^Y),n(X)]).
%N -> N PP
rule(n(X^Z),[n(X^Y),pp((X^Y)^Z)]).
%PP -> P NP
rule(pp(Z),[p(X^Y^Z),np(X^Y)]).
%VP -> IV
rule(vp(X,WH),[iv(X,WH)]).
%NP -> PN
rule(np(A),[pn(A)]). %proper noun
%NP -> N
rule(np((X^Q)^exists(X,and(P,Q))), [n(X^P)]).
%Y -> WHPR VP  Who saw Tom
rule(Y,[whpr(X^Y),vp(X,[])]).
%INV_S -> AUX NP VP
rule(inv_s(Y,[WH]),[aux, np(X^Y),vp(X,[WH])]).
%INV_S -> AUX NP TV
%rule(inv_s(Y,WH),[aux, np(X^Y),iv(X,WH)]).
rule(Z,[whpr((X^Y)^Z), inv_s(Y,[X])]).
%ynq -> AUX NP VP Did the ham expire
rule(ynq(Y),[aux, np(X^Y),vp(X,[])]).
%ynq -> Is the ham inside the box
rule(ynq(Z),[be,np(X^Y),pp((X^Y)^Z)]).
%be  -> be there
rule(be,[be,there]).
%is there a blue box
rule(ynq(C),[be,np(_^C)]).
%RC -> AUX NP VP
rule(rc(Y,[A]),[rel,np(X^Y),vp(X^A,[])]).
%TV -> IV
rule(iv(X^Z, [Y]), [tv(X^Y^Z, [])]).
%TV -> TV
rule(tv(Y^Z, [X]), [tv(X^Y^Z, [])]).
% N -> N RC
rule(n(X^and(Y, Z)), [n(X^Y), rc(X^Z, [])]).
rule(n(X^and(Y, Z)), [n(X^Y), rc(Z, [X])]).
% RC -> REL S
rule(rc(P, [X]),[rel,s(P,[X])]).
%NP -> vacp NP
rule(np(X),[vacp,np(X)]).
%S -> THERE BE NP
rule(s(Y),[there,be,np(_^Y)]).
% ...


% ===========================================================
%  Modelchecker:
%  1. If input is a declarative, check if true
%  2. If input is a yes-no question, check if true
%  3. If input is a content question, find answer
% ===========================================================
%

model([box1,box2,box3,box4,banana1,ham1bowl1,s,egg1,sam1,t,f,milk1,shelf1],
           [ [box, [box1,box2,box3,box4]],
             [thing,[box1,box2,box3,box4,ham1,milk1,egg1,bowl1,banana1,f,shelf1]],
             [milk,[milk1]],
             [person,[s,t,sam1]],
             [egg,[egg1]],
             [ham,[ham1]],
             [banana,[banana1]],
             [blue, [box1]],
             [green, [box2]],
             [yellow, [box3]],
             [white, [bowl1,box4]],
             [skim, [milk1]],
             [freezer, [f]],
             [bowl, [bowl1]],
             [sue, [s]],
             [tom, [t]],
             [sam, [sam1]],
             [contain,[ [box1,ham1],[box2,banana1],[f,box4] ]],
             [in,[ [ham1,box1] ]],
             [drink,[ [sam1,milk1] ]],
             [on,[ [ham3,bowl1] ]],
             [belong,[ [box1,s] ]],
             [put,[[s,box3,bowl1]]]
           ]).


%Question rules
modelchecker(q(F,K),Y):-
 sat([],exists(F,K),Y),!.

modelchecker(q(_,_),[wh_interrogative_false]).

%Return true if the ynq is true
modelchecker( ynq( F ), [yes_to_question]):-
   sat([],F,_),!.
%Return false if not true
modelchecker(ynq(_),[no_to_question]).
%Return if true
modelchecker(s(X,[]),[true_in_the_model]):-
 sat([],X,_),!.
%Return if false
modelchecker(s(_,[]),[false_in_the_model]).

modelchecker(SemanticRepresentation,Y):-
 sat([],SemanticRepresentation,Y).

% ==================================================
% Function i
% Determines the value of a variable/constant in an assignment G
% ==================================================

i(Var,G,Value):-
    var(Var),
    member([Var2,Value],G),
    Var == Var2.

i(C,_,Value):-
   nonvar(C),
   f(C,Value).


% ==================================================
% Function F
% Determines if a value is in the denotation of a Predicate/Relation
% ==================================================

f(Symbol,Value):-
   model(_,F),
    member([Symbol,ListOfValues],F),
    member(Value,ListOfValues).


% ==================================================
% Extension of a variable assignment
% ==================================================

extend(G,X,[ [X,Val] | G]):-
   model(D,_),
   member(Val,D).


% ==================================================
% Existential quantifier
% ==================================================

sat(G1,exists(X,Formula),G3):-
   extend(G1,X,G2),
   sat(G2,Formula,G3).



% ==================================================
% Definite quantifier (semantic rather than pragmatic account)
% ==================================================

 sat(G1,the(X,and(A,B)),G3):-
   sat(G1,exists(X,and(A,B)),G3),
   i(X,G3,Value),
   \+ ( ( sat(G1,exists(X,A),G2), i(X,G2,Value2), \+(Value = Value2)) ).






% ==================================================
% Negation
% ==================================================

sat(G,not(Formula2),G):-
   \+ sat(G,Formula2,_).

% ==================================================
% Universal quantifier
% ==================================================

sat(G, forall(X,Formula2),G):-
  sat(G,not( exists(X,not(Formula2) ) ),G).


% ==================================================
% Conjunction
% ==================================================

sat(G1,and(Formula1,Formula2),G3):-
  sat(G1,Formula1,G2),
  sat(G2,Formula2,G3).


% ==================================================
% Disjunction
% ==================================================


sat(G1,or(Formula1,Formula2),G2):-
  ( sat(G1,Formula1,G2) ;
    sat(G1,Formula2,G2) ).


% ==================================================
% Implication
% ==================================================

sat(G1,imp(Formula1,Formula2),G2):-
   sat(G1,or(not(Formula1),Formula2),G2).


% ==================================================
% Predicates
% ==================================================

sat(G,Predicate,G):-
   Predicate =.. [P,Var],
   \+ (P = not),
   i(Var,G,Value),
   f(P,Value).

% ==================================================
% Two-place Relations
% ==================================================

sat(G,Rel,G):-
   Rel =.. [R,Var1,Var2],
   \+ ( member(R,[exists,forall,and,or,imp,the]) ),
   i(Var1,G,Value1),
   i(Var2,G,Value2),
   f(R,[Value1,Value2]).


% ===========================================================
%  Respond
%  For each input type, react appropriately.
% ===========================================================

% Declarative true in the model
respond(Evaluation) :-
		Evaluation = [true_in_the_model],
		write('That is correct'),!.

% Declarative false in the model
respond(Evaluation) :-
		Evaluation = [false_in_the_model],
		write('That is not correct'),!.

% Yes-No interrogative true in the model
respond(Evaluation) :-
		Evaluation = [yes_to_question],
		write('yes').

% Yes-No interrogative false in the model
respond(Evaluation) :-
		Evaluation = [no_to_question],
		write('no').

% wh-interrogative true in the model
respond(Evaluation):-
 last(Evaluation,[_|Z2]), %Get the last element of the list
 model(_,B),
 findall(H, (member([H|T],B),member(Z2,T),write(H),nl),K),!.


% wh-interrogative false in the model
% ...
respond(Evaluation) :-
		Evaluation = [wh_interrogative_false],
		write("no").


























