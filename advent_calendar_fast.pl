:- module(advent_calendar, [
    advent_calendar/1]).

% A smarter, faster solver for the puzzle described in
% https://groups.google.com/forum/#!topic/swi-prolog/Q48iqUTw7dw

:- use_module(library(dialect/sicstus/lists), [
    sublist/2]).

% True iff Doors is a sorted list containing exactly 8 integers from 1 to 24
% such that for each integer from 1 to 23, there are two numbers in the list
% with that distance. Some solutions may be found more than once.
advent_calendar(Doors) :-
  ak(23, [], Doors).

% Recursively go from 23 to 1 (stop at 0), at each step adding to the list of
% open doors (or making sure that there already is) a pair of open doors with
% the given distance. Fail whenever that results in more than 8 open doors.
% Case 1: no further distances to add.
ak(0, Doors0, Doors) :-
  !,
  % At this point we know that the distances requirement is fulfilled, now we
  % need to make sure the list length is 8 by adding arbitrary numbers until
  % that is the case.
  length(Doors0, Length),
  NumToAdd is 8 - Length,
  length(ToAdd, NumToAdd),
  findall(Number, ( between(1, 24, Number), \+ member(Doors0, Number) ), Choices),
  sublist(ToAdd, Choices),
  append(ToAdd, Doors0, Doors1),
  sort(Doors1, Doors).
% Case 2: there is already a pair with the current distance.
ak(Distance, Old, Doors) :-
  append(_, [Lo|Rest], Old),
  Hi is Lo + Distance,
  member(Hi, Rest),
  !,
  % Continue with next distance.
  NextDistance is Distance - 1,
  ak(NextDistance, Old, Doors).
% Case 3: there is no pair yet with the current distance.
ak(Distance, Old, Doors) :-
  % Select a pair of numbers with the given distance:
  MaxLo is 24 - Distance,
  between(1, MaxLo, Lo),
  Hi is Lo + Distance,
  % Add them to the list:
  sort([Hi, Lo|Old], New),
  % Make sure the list is not too long:
  length(New, Length),
  Length =< 8,
  % Continue:
  NextDistance is Distance - 1,
  ak(NextDistance, New, Doors).

:- begin_tests(advent_calendar_simple).

test(solutions) :-
  setof(Doors, advent_calendar(Doors), [
      [1,2,3,12,16,19,22,24],
      [1,2,5,11,17,19,22,24],
      [1,3,6,8,14,20,23,24],
      [1,3,6,9,13,22,23,24]]).

:- end_tests(advent_calendar_simple).
