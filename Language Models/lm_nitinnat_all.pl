:- [bigram_nitinnat]. :- [unigram_nitinnat].

%Represent the number of words in the vocabulary as a fact.
vocab(14804).
%Wrappers around bigram and unigram facts
%to accept non-existent bigrams and unigrams.
bi_calc(W1,W2,X) :-
    bigram(X,W1,W2),!.
bi_calc(_,_,0).

uni_calc(W1,X) :-
    unigram(X,W1),!.
uni_calc(_,_,0).


%Wrapper function for the calculation of probability.
calc_prob(ListOfWords,SmoothedLog10Probability):-
    calc_prob(ListOfWords,0,SmoothedLog10Probability).

%Calculate the probability using an accumulator.
calc_prob([_],N,N).
calc_prob([W1,W2|L], Accum , N) :-
    bi_calc(W1,W2,X),
    uni_calc(W1,Y),
    vocab(V),
    log10((X+1)/(Y+V),P1),
    P2 is Accum + P1,
    calc_prob( [W2|L], P2, N).



%Alpha smoothing
alpha(0.5).
calc_prob_alpha(ListOfWords,SmoothedLog10Probability):-
    calc_prob_alpha(ListOfWords,0,SmoothedLog10Probability).

calc_prob_alpha([_],N,N).
calc_prob_alpha([W1,W2|L], Accum , N) :-
    bi_calc(W1,W2,X),
    uni_calc(W1,Y),
    vocab(V),
    alpha(A),
    log10((X+A)/(Y+(A*V)),P1),
    P2 is Accum + P1,
    calc_prob( [W2|L], P2, N).


