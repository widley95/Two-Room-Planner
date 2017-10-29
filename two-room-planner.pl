%%%%%%%%%%%%%%% Two-Rooms Block Planner %%%%%%%%%%%%%%%%%%
%%%
%%% Julian Quitian, Ley Nezifort
%%%
%%% CAP 4630 - Artificial Intelligence
%%% University of Central Florida
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module( planner,
	   [
	       plan/4,change_state/3,conditions_met/2,member_state/2,
			 move/3,go/2,test/0, test1/0,test2/0
	   ]).

/* Load the utilities provided */
:- [utils].

/* If the current state and goal are the same... */
/*      Print the moves                          */
plan(State, Goal, _, Moves) :-	equal_set(State, Goal),
				write('moves are'), nl,
				reverse_print_stack(Moves).
plan(State, Goal, Been_list, Moves) :-
                /* Blindly make a move */
				move(Name, Preconditions, Actions),
                /* Check if the preconditions for that move are true */
				conditions_met(Preconditions, State),
                /* If you made it this far then the move is possible */
				change_state(State, Actions, Child_state),
                /* Make sure the new state is not one we already did */
				not(member_state(Child_state, Been_list)),
                /* Add new state to the list of been states */
				stack(Child_state, Been_list, New_been_list),
                /* Add the new move to the list */
				stack(Name, Moves, New_moves),
            /* Plan your next move */
			plan(Child_state, Goal, New_been_list, New_moves),!.

/* Change the state from a given list of moves */
/* If the list is empty then don't do anything */
change_state(S, [], S).
/* Tail recursively apply the moves to the state */
/*      NOTE: The add and del are simply strings from the move predicate */
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new), !.
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new), !.
/* Check if the preconditions P are in the current state S */
conditions_met(P, S) :- subset(P, S).

member_state(S, [H|_]) :-	equal_set(S, H).
member_state(S, [_|T]) :-	member_state(S, T).

/* move types */
/* move(Name, 
*       [Preconditions], 
*       [Moves]). */
move(pickup(X),
        [handempty, clear(X), on(X, Y, Z), currentroom(Z)],
		[del(handempty), del(clear(X)), del(on(X, Y, Z)), add(clear(Y)),	add(holding(X))]).

move(pickup(X),
        [handempty, clear(X), ontable(X, Z), currentroom(Z)],
		[del(handempty), del(clear(X)), del(ontable(X, Z)), add(holding(X))]).

move(putdown(X),
        [holding(X), currentroom(Z)],
		[del(holding(X)), add(ontable(X, Z)), add(clear(X)), add(handempty)]).

move(stack(X, Y),
        [holding(X), clear(Y), currentroom(Z)],
		[del(holding(X)), del(clear(Y)), add(handempty), add(on(X, Y, Z)), add(clear(X))]).

move(goroom(1),
        [currentroom(2)],
        [del(currentroom(2)), add(currentroom(1))]).

move(goroom(2),
        [currentroom(1)],
        [del(currentroom(1)), add(currentroom(2))]).

/* run commands */
go(S, G) :- plan(S, G, [S], []).

/* A stacked on B in room 1, nothing in room 2. Goal: A stacked on B in room 2 */
test :- go([handempty, ontable(b,1), on(a, b, 1), clear(a), currentroom(1)],
		    [handempty, ontable(b,2), on(a, b, 2), clear(a), currentroom(1)]).

/* Same as test, except starting crane position at room 2. End at room 1 */
test1 :- go([handempty, ontable(b,1), on(a, b, 1), clear(a), currentroom(2)],
			[handempty, ontable(b,2), on(a, b, 2), clear(a), currentroom(1)]).

/* Switch element tables. Begin and end at room 1 */
test2 :- go([handempty, ontable(h,1), ontable(i,1), clear(h), clear(i), currentroom(2)],
				[handempty, ontable(h,2), ontable(i,2), clear(h), clear(i), currentroom(1)]).
