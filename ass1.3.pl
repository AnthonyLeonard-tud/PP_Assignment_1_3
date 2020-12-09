%% ass1.3.pl

print_status([Row|Rows]) :- atomic_list_concat(Row, '|', TRow), string_concat('|', TRow, Out1), string_concat(Out1, '|', Out2), writeln(Out2), print_status(Rows).

print_status_nonBlocking([]):-true.
print_status_nonBlocking([Row|Rows]) :- atomic_list_concat(Row, '|', TRow), string_concat('|', TRow, Out1), string_concat(Out1, '|', Out2), writeln(Out2), print_status_nonBlocking(Rows).

positionInList([Element|_], Element, 0).
positionInList([_|Tail], Element, Pos):- positionInList(Tail, Element, Pos1), Pos is Pos1+1. 

high([List| R], Element, Height) :- member(Element,List), positionInList(List,Element,Height).
high([List| R], Element, Height) :- high(R, Element, Height).

all_same_height(Blocks, Height, Results) :- findall(Elem, high(Blocks, Elem,Height), Results).

same_height(Blocks, X, Y) :- high(Blocks, X, H1), high(Blocks, Y, H2), H1=:=H2.


rowN([H|_],1,H):-!.
rowN([_|T],I,X) :- I1 is I-1, rowN(T,I1,X).

columnN([],_,[]).
columnN([H|T], I, [R|X]):- rowN(H, I, R), columnN(T,I,X).

last(X,[X]).
last(X,[_|Z]) :- last(X,Z).

without_last([_], []).
without_last([X|Xs], [X|WithoutLast]) :- without_last(Xs, WithoutLast).

replace_nth0(List, Index, OldElem, NewElem, NewList):-nth0(Index,List,OldElem,Transfer),nth0(Index,NewList,NewElem,Transfer).
replace_nth1(List, Index, OldElem, NewElem, NewList):-nth1(Index,List,OldElem,Transfer),nth1(Index,NewList,NewElem,Transfer).

moveblock(Blocks, Elem, L1, L2, Blocks2 ):-
    print_status_nonBlocking(Blocks),
    rowN(Blocks,L1,Out1),
    member(Elem,Out1), 
    last(LastElem, Out1), 
    LastElem == Elem, 
    without_last(Out1, Out1New), 
    rowN(Blocks,L2,Out2), 
    append(Out2, [Elem], Out2New), 
    replace_nth1(Blocks, L1, _, Out1New, Blocks1), 
    replace_nth1(Blocks1, L2, _, Out2New, Blocks2),
    print_status_nonBlocking(Blocks2).

%% options
%% 1)
%% Brute Force, tentative strategy... 
%%      1) find "a" - head of goal list [a,b,c,d,e,f,g]), note height and stack id 
%%      2) is it on bottom,
%%          N
%%              3) if not move all above it to the fullest stack, 
%%              4) move all on emptiest stack onto the fullest stack
%%              5) move "a" to floor of now empty stack
%%              6) change to "b" and repeat...  
%%          Y
%%              7) if yes, clear all above it
%%              8) change to "b" and repeat ...
%% 
%% 2)
%% working both ends 
%% similar to above, but also looking at solving from g in reverse order at the same time...
%%
%% 3)
%% working from optimum end
%% checking and seeing which end might be more efficient to start from, 
%% i.e. 
%% g lower than a...
%% g less blocks above than a
%%
%% 4)
%% looking at blocks in pairs, so (a,b) finding both, clearing a path to both, and moving as nessary
%% then moving on to (c,d) pair

%% Brute Force

%% solution list
solutionList:-[a,b,c,d,e,f,g,h].

%% find element
at(Mat, Row, Col, Val) :- nth1(Row, Mat, ARow), nth1(Col, ARow, Val).
at1(Mat, Row, Col, Val) :- nth1(Row, Mat, ARow), nth0(Col, ARow, Val).

%% from https://stackoverflow.com/questions/29868770/swi-prolog-matrix-how-to-find-the-value-of-an-element-by-indexes
%% at([[a,b,c],[d,e,f],[g,h]],Y,X,g).
%% Y = 3,
%% X = 1 ;
%% note X is in the wrong base!, fixed in "at1"

%% [[b,c,f],[a,d,g],[h,e]]
%% 
%% order_blocks([[b,c,f],[a,d,g],[h,e]], [a,b,c,d,e,f,g,h],N).

%% test cases
%% order_blocks([[],[a],[]], [a],N).

%% order_blocks([[],[a],[b]], [a,b],N).
%% order_blocks([[],[b,a],[]], [a,b],N).
%% order_blocks([[],[a,b],[]], [a,b],N).

/*
initial 
order_blocks(B, [H|T],N):-
    Ntemp is 0,
    CounterTemp is 0,
    at1(B,Stack,Position,H),
    writeln(Stack),
    writeln(Position),
    Position == CounterTemp, 
    Counter is CounterTemp + 1,
    order_blocks(B,[T],Ntemp, Counter )
    .

*/

%% new strategy
%% true brute force... 
%% move all blocks to stack A
%% find first elemt, move all blockers to c, move element to b
%% move all elemt on c back to a, repeat with next list item...

%% moveAllBlocksToA([H|T], NumberOfMoves, RowToMoveTo):-RowToMoveFromTemp is RowToMoveTo + 1, rowN([T|_],RowToMoveFromTemp,[RowH|RowT]),moveRow, moveblock(Blocks, Elem, RowToMoveTo, RowToMoveFromTemp ).

list_reverse([],[]).
list_reverse([A|B],C):-list_reverse(B,D), concatenate(D, [A], C).

concatenate([],A,A).
concatenate([A|B], C, [A|D]):-concatenate(B,C,D). 

%% moveAllBlocksToRow1([[b,c,f],[a,d,g],[h,e]] , N, 1 ).
moveAllBlocksToRow1(B, N, RowToMoveTo):-
    RowToMoveFromTemp is RowToMoveTo + 1,
    moveOtherRows(B,N,RowToMoveFromTemp,RowToMoveTo),
    writeln('CCC').

moveOtherRows(B,N,RowToMoveFromTemp,RowToMoveTo):-
    rowN(B,RowToMoveFromTemp,X), 
    writeln(X),
    list_reverse(X,Y),
    writeln(Y),
    moveRowElements(B,Y,RowToMoveFromTemp, RowToMoveTo),
    writeln('BBB').

moveRowElements(B,[],_,_).
moveRowElements(B,[H|T], RowToMoveFromTemp, RowToMoveTo):-
    writeln(H),
    writeln(T),
    moveblock(B, H, RowToMoveFromTemp, RowToMoveTo, Out ), 
    writeln(T), 
    moveRowElements(Out,T, RowToMoveFromTemp, RowToMoveTo),
    writeln('AAA').

%% order_blocks([[b,c,f],[a,d,g],[h,e]], [a],N).
order_blocks(B, [H|T],N):- 
    moveAllBlocksToRow1(B, N, 1)
    .