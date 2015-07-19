:- module(advent_calendar_simple, [
    advent_calendar/1]).

% A generate-and-test solver for the puzzle described in
% https://groups.google.com/forum/#!topic/swi-prolog/Q48iqUTw7dw

% True iff Doors is a sorted list containing exactly 8 integers from 1 to 24
% such that for each integer from 1 to 23, there are two numbers in the list
% with that distance.
advent_calendar(Doors) :-
  choose(24, 8, Chosen),
  forall(between(1, 23, Distance), find_distance(Distance, Chosen)),
  reverse(Chosen, Doors).

choose(_, 0, []) :-
  !.
choose(N, K, [N|Chosen]) :-
  N > 0,
  M is N - 1,
  J is K - 1,
  choose(M, J, Chosen).
choose(N, K, Chosen) :-
  N > 0,
  M is N - 1,
  choose(M, K, Chosen).

find_distance(Distance, Numbers) :-
  append(_, [Minuend|Rest], Numbers),
  append(_, [Subtrahend|_], Rest),
  Minuend - Subtrahend =:= Distance.

:- begin_tests(advent_calendar_simple).

test(solutions) :-
  setof(Doors, advent_calendar(Doors), [
      [1,2,3,12,16,19,22,24],
      [1,2,5,11,17,19,22,24],
      [1,3,6,8,14,20,23,24],
      [1,3,6,9,13,22,23,24]]).

:- end_tests(advent_calendar_simple).
