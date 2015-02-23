:- module(ohhi, [
    ohhi/1,
    pretty_print/1]).

:- use_module(library(clpfd)).

%%% LOGIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	ohhi(?Rows)
%
%	True iff Rows is a solution of the game http://0hh1.com/, with the board
%	represented as a list of lists (rows).
ohhi(Rows) :-
  % Board shape:
  length(Rows, Length),
  Length mod 2 =:= 0,
  maplist(length_(Length), Rows),
  % Equal numbers in each row/col
  transpose(Rows, Cols),
  Num is Length / 2,
  maplist(equal_numbers(Num), Rows),
  maplist(equal_numbers(Num), Cols),
  % No three like neighbors:
  maplist(no3same, Rows),
  maplist(no3same, Cols),
  % Generate:
  flatten(Rows, Cells),
  label(Cells),
  % All rows and cols must be different. This can't be expressed using
  % constraints, can it?
  test_all_different(Rows),
  test_all_different(Cols).

equal_numbers(Num, Row) :-
  global_cardinality(Row, [1-Num, 2-Num]). % 1 for red, 2 for blue

no3same([A, B, C|Rest]) :-
  !,
  A #= B #==> B #\= C,
  no3same([B, C|Rest]).
no3same(_).

test_all_different(Rows) :-
  forall(
      ( append(_, [First|Rest], Rows)
      ),
      ( \+ member(First, Rest)
      ) ).

length_(Length, List) :-
  length(List, Length).

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
              ansi_format([bg(Color)], '~w', [String])
            ) ),
        nl ) ).

cell_color(1, red).
cell_color(2, blue).

cell_string(1, '+').
cell_string(2, '-').
