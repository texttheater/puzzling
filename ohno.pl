:- module(ohno, [
    ohno/1,
    pretty_print/1]).

:- use_module(library(clpfd)).

%%% LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	ohno(?Rows)
%
%	True iff Rows is a solution of the game http://0hno.com on a square
%	board at most 11 tiles wide and high, represented as a list of lists
%	(rows) in which red dots are represented by `0` and blue dots are
%	represented by the number of other blue dots they can see.
ohno(Rows) :-
  % Board shape:
  between(1, 11, Length), % limit for the benefit of pretty-printing
  square(Length, Rows),
  % Determine minimum and maximum numbers for each dot:
  Max is (Length - 1) * 2,
  maplist(maplist(seemax(Max)), Rows),
  % Make four matrixes that indicate how many blue dots each dot sees in each
  % direction, i.e. in the East...
  maplist(row_seen, Rows, EastMatrix),
  % ...West...
  maplist(reverse, Rows, ReversedRows),
  maplist(row_seen, ReversedRows, ReversedWestMatrix),
  maplist(reverse, ReversedWestMatrix, WestMatrix),
  % ...South...
  transpose(Rows, Columns),
  maplist(row_seen, Columns, TransposedSouthMatrix),
  transpose(TransposedSouthMatrix, SouthMatrix),
  % ...and North.
  maplist(reverse, Columns, ReversedColumns),
  maplist(row_seen, ReversedColumns, ReversedTransposedNorthMatrix),
  maplist(reverse, ReversedTransposedNorthMatrix, TransposedNorthMatrix),
  transpose(TransposedNorthMatrix, NorthMatrix),
  % Sum up the four directions for each dot:
  maplist(maplist(sum), EastMatrix, WestMatrix, SouthMatrix, NorthMatrix, Rows),
  % Generate a ground solution:
  flatten(Rows, Cells),
  label(Cells).

sum(East, West, South, North, Dot) :-
  Dot #= East + West + South + North.

seemax(Max, Dot) :-
  Dot in 0..Max.

square(Length, Rows) :-
  length(Rows, Length),
  maplist(length_(Length), Rows).

length_(Length, List) :-
  length(List, Length).

%%	row_seen(+Row, -Seen)
%
%	Given a row, indicates how many blue dots each dot in the row sees on
%	its right (red dots are blind and don't see anything).
row_seen([_], [0]) :-
  !.
row_seen([First,Second|Rest], [FirstSees,SecondSees|RestSee]) :-
  row_seen([Second|Rest], [SecondSees|RestSee]),
  % If the second dot is blue and so is the first, the first sees one more blue
  % dot than the second on the right:
  (    First #> 0
  #/\  Second #> 0
  #==> FirstSees #= SecondSees + 1
  ),
  % If the second is red, it blocks the first dot's view and it doesn't see any
  % blue dots on the right:
  (    Second #= 0
  #==> FirstSees #= 0
  ).

%%% PRETTY PRINTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pretty_print(Rows) :-
  forall(
      ( member(Row, Rows)
      ),
      ( forall(
            ( member(Cell, Row)
            ),
            ( cell_color(Cell, Color),
              cell_string(Cell, String),
              ansi_format([fg(Color)], '~w', [String])
            ) ),
        nl ) ).

cell_color(0, red) :-
  !.
cell_color(_, blue).

cell_string(0, ●) :-
  !.
cell_string(N, String) :-
  nth1(N, [❶, ❷, ❸, ❹, ❺, ❻, ❼, ❽, ❾, ❿, ⓫, ⓬, ⓭, ⓮, ⓯, ⓰, ⓱, ⓲, ⓳, ⓴],
      String).

%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%      maplist(:Goal, ?List1, ?List2, ?List3, ?List4)
%
%       As maplist/2, operating on  quintuples   of  elements  from four
%       lists.

maplist(Goal, List1, List2, List3, List4, List5) :-
        maplist_(List1, List2, List3, List4, List5, Goal).

maplist_([], [], [], [], [], _).
maplist_([Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3], [Elem4|Tail4], [Elem5|Tail5], Goal) :-
        call(Goal, Elem1, Elem2, Elem3, Elem4, Elem5),
        maplist_(Tail1, Tail2, Tail3, Tail4, Tail5, Goal).

