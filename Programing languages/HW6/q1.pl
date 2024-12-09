:- dynamic add_bt/4.

bt(X, []).

bt_aux(bt(_, []), 0). 
bt_aux(bt(X, [bt(Y,Z)|YS]), K) :-
    X =< Y, 
    bt_aux(bt(Y,Z), K1), 
    bt_aux(bt(X, YS), K1),
    K is K1 + 1. 


len([], 0). 
len([_ | XS], K) :- 
    len(XS, K1), 
    K is K1 + 1. 


bt(X, [Y|YS]) :- 
    bt_aux(bt(X, [Y|YS]), K), 
    len([Y|YS], N), 
    K =:= N. 

merge_bt([], [], []).
merge_bt(bt(X, XS), bt(Y, YS), bt(X, [bt(Y, YS) | XS])) :- 
    X =< Y,
    len(XS,K1),
    len(YS,K2),
    K1 =:= K2,
    !.
merge_bt(bt(X, XS), bt(Y, YS), bt(Y, [bt(X, XS) | YS])) :- 
    len(XS,K1),
    len(YS,K2),
    K1 =:= K2,
    Y < X.

add_bt1(bt(X, XS), [], K, [bt(X, XS)]) :-
    len(XS, Rank),
    Rank =:= K.  
add_bt1(bt(X, XS), [], K, [empty|NewRest]) :-
    len(XS, Rank),
    Rank > K,
    NewK is K + 1,
    add_bt1(bt(X, XS),[empty],NewK,NewRest). 
     
add_bt1(bt(X, XS), [empty | Rest], K, [bt(X, XS) | Rest]) :-
    len(XS, Rank),
    Rank =:= K. 

add_bt1(bt(X, XS), [empty | Rest], K, [empty | NewRest]) :-
    len(XS, Rank),
    Rank > K,  
    NewK is K+1,
    add_bt1(bt(X, XS), Rest, NewK, NewRest).

add_bt1(bt(X, XS), [bt(Y, YS) | Rest], K, [empty | NewRest]) :-
    len(XS, RankX),
    len(YS, RankY),
    RankX =:= RankY, 
    merge_bt(bt(X, XS), bt(Y, YS), MergedTree),
    NewK is K + 1,
    add_bt1(MergedTree, Rest, NewK, NewRest).

add_bt1(bt(X, XS), [bt(Y, YS) | Rest], K, [bt(Y, YS) | NewRest]) :-
    len(XS, RankX),
    len(YS, RankY),
    RankX =\= RankY,  
    NewK is K + 1,  
    add_bt1(bt(X, XS), Rest, NewK, NewRest).


add_bt(bt(X, XS), Heap, NewList) :-
    len(XS, K), 
    add_bt1(bt(X, XS), Heap, 0, NewList).
add(X,Heap,NewList) :-  add_bt(bt(X,[]),Heap,NewList).


findMin(X, [bt(X, _)]).


findMin(Min, [empty | Rest]) :-
    findMin(Min, Rest).


findMin(Min, [bt(Y, _) | Rest]) :-
    findMin(RestMin, Rest),
    Min is min(Y, RestMin).


findMin(inf, []). 
removeMin(X,[bt(X,XS)],[],XS) :- !.
removeMin(X,[bt(X,XS)|Rest],[empty|Rest],XS).
removeMin(X,[bt(Y,YS)|Rest],[bt(Y,YS)|NewRest],XS) :-
     Y =\= X ,
    removeMin(X,Rest,NewRest,XS).
removeMin(X,[empty|Rest],[empty|NewRest],XS) :-
   removeMin(X,Rest,NewRest,XS).
addList([], Heap, Heap).
addList([X|XS],Heap,NewHeap) :-
    add_bt(X,Heap,Heap1) ,
    addList(XS,Heap1,NewHeap),!.
fetch_min(X, [], []) :- !, fail. 
fetch_min(X, Heap, NewHeap) :-
    findMin(X, Heap),
    removeMin(X, Heap, Heap1, List),
    addList(List, Heap1, NewHeap),!.
make_heap([],Heap,Heap).
make_heap([X|XS],Heap,NewHeap) :-
    add(X,Heap,Heap1),
    make_heap(XS,Heap1,NewHeap).
make_list([], []). 
make_list(Heap, [X | XS]) :-
    fetch_min(X, Heap, Heap1), 
    make_list(Heap1, XS).   
sort_me(List,SortedList) :-
    make_heap(List,[],NewHeap),
    make_list(NewHeap,SortedList).
