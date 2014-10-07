:- module(terminator,[ggp_terminator_player/5]).
%-----------------------------------------------------------------------------
% General Game Player environment for
% Methods of AI 2009

%
%-----------------------------------------------------------------------------
%
% name convention:
%   <file name> = <player name>.pl
%   exported precicate:
%    ggp_<player name>_player(
%         +State,       % sorted list of atoms
%         +Role,        % role of the layer
%         -(Role:Move), % move, must be legal move
%         +Time,        % time limit in seconds
%         +MovesList    % history: list of lists of
%         )             %  the moves of the players
%                       %  (most recent first)
%
%-----------------------------------------------------------------------------
% imported predicates:
%
% gdl predicates with specified semantics:
%
%   gdl_role(?Role)
%   gdl_init(?Atom)
%   gdl_next(?Atom, +State, +Moves)
%   gdl_true(?Atom, +State)
%   gdl_does(?Role, ?Action, +State, +Moves)
%   gdl_terminal(+State)
%   gdl_goal(+Role, -Utility, +State)
%
% other useful predicates:
%
%   ggp_next_state(+State1, +Moves, -State2)
%
%-----------------------------------------------------------------------------
% gdl_init(?Atom)
%
% this predicate is true if the partial state description in 'Atom' holds in
% the initial state.
%-----------------------------------------------------------------------------
% gdl_role(?Role)
%
% this predicates enumerates the game-specific player symbols.
%-----------------------------------------------------------------------------
% gdl_legal(+Player, ?Move, +State)
%
% enumerates all possible moves for the 'Player' in the current 'State'
% a state is a list of Atoms
% a move in this case is just a move and not a structure of the form
% 'Player:Move'
%-----------------------------------------------------------------------------
% gdl_true(+Atom, +State)
%
% determines if condition 'Atom' holds in the current State.
% a state is a list of Atoms
%-----------------------------------------------------------------------------
% gdl_next(?Atom, +State, +Moves)
%
% enumerates the atoms that hold in the next state after application of the
% 'Moves'.
% 'Moves' contains a list of actions that are taken from the players in the
% current turn. The list contains entries of the form 'Role:action'
% a state is a list of atoms
%-----------------------------------------------------------------------------
% ggp_next_state(+State1, +Moves, ?State2)
%
% 'State2' is the complete State description acquired by 'gdl_next'
%-----------------------------------------------------------------------------
% gdl_terminal(+State)
%
% succeeds if a state is a terminal state.
%-----------------------------------------------------------------------------
% gdl_does(?Role, ?Action, +State, +Moves)
%
% enumerates all actions players take, given a state and move decisions
%-----------------------------------------------------------------------------
% gdl_goal(+Role, -Utility, +State)
%
% succeeds on winning states for 'Role' with a 'Utility' of 100
% succeeds on draw states for with a 'Utility' of 50
% succeeds on losing states for 'Role' with a 'Utility' of 0
%
%-----------------------------------------------------------------------------


%-------------------------------------------------------------
% Initialize
ggp_terminator_player([], Role, _Move, _Time, _MovesList) :-
   assert(alpha(-9999,0)), assert(beta(9999,0)), % Initial value for alpha and beta
   assert(depth(0)), % Initial value for our number of exploration-steps in the State Space
   asserta(isMax(Role)), % (we suppose we are entered as first in the player arguments when launching the game
     % determine opponent :
       findall(OneRole, gdl_role(OneRole), Roles),
       select(Role, Roles, OpponentRoles),
       member(Opponent, OpponentRoles),
       asserta(isMin(Opponent)).


% Determine a move
ggp_terminator_player(State, Role, Move, _Time, _MovesList) :-
  ggp_store(Role, db_flag(0)),
  findall(Role:Move1, gdl_legal(Role, Move1, State), MoveAlt),					     value(State, _Value, Index, Role),% This apply Minimax with alpha-beta-pruning
  % and will give us the Index of the best move
  nth0(Index, MoveAlt, Move),
  ggp_db(1, 'legal moves':Role:MoveAlt).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SECTION for MINIMAX with alpha-beta pruning
%% implementation :                       %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% value(+Node, -Value, -Index, +Player)  %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gives out, among the Successor-states of 'Node', the Value and
%  Index of the best move for 'Player' (according to Minimax)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
value(Node, Value, Index, Player):-
	isMax(Player),!,max_value(Node,Value,Index, Player)
	;
	isMin(Player),!,min_value(Node, Value,Index, Player).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% max_value(+Node, -Value, -Index, +Player)  %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is one of the 2 auxiliary functions for the 'value/4'
% function: it will be called when 'Node' is a 'Max' node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
max_value(Node, Value,Index, Player):-
	%If we are in a terminal state, then simply read the Value...
  	gdl_terminal(Node), % check whether it is a terminal state
	gdl_goal(Player, Value, Node), ! % get the value of the goal state

	%...otherwise, we have to expand the Node, and take the max value of the children:
	;
	(   expand(Player, Node,Successors),
	 depth(X),Y is X+1,retract(depth(_)), asserta(depth(Y)),%Book-keeping to know at which
	                                                        %level we are.
	isMin(Min),%(reading what our opponent is called)
	value_list(Successors,[],Values,  Min),% we'll get the 'Values' of the 'Successors' states
	max_list(Values, Value)), % take the max of these Values
	nth0(Index,Values,Value), !. % and bind the corresponding Index
				  %(if the same Value appears several times, just take
				  % the first, and don't backtrack).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% min_value(+Node, -Value, -Index, +Player)  %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is is perfectly analogical to max_value
% only for the 'min' case
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min_value(Node, Value,Index, Player):-
	%if we can read directly the Value (terminal state), then simply read it:
  	gdl_terminal(Node), % check whether it is a terminal state
	gdl_goal(Player, Value, Node), ! % get the value of the goal state

	%...otherwise, we have to expand the Node, and take the max value of the children:
	;
	( expand(Player, Node,Successors),
	depth(X),  Y is X+1, retract(depth(_)),asserta(depth(Y)),% for the comments here and below
	                                                          % please see in 'max_value/4'
	isMax(Max),
	value_list(Successors,[],Values, Max),
	min_list(Values, Value)),
	nth0(Index,Values,Value), !.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% value_list(+NodeList,-ValueListPartial,-ValueList, +Player)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constructs into 'ValueList' the list of values corresponding
%  to the given 'NodeList'.
% Use a 'ValueListPartial' accumulator.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Stop case:
value_list([],ValueListPartial,ValueList, Player):-
	reverseMy(ValueListPartial, ValueList),%This is nice to have the list of values
	                                       % in the same order as the nodes (this is not
					       % the case b/c of the accumulator),
					       % but this is not necessary actually (later we get
					       % the index of this best value)
	depth(Depth),DepthPrevious is Depth-1, retract(depth(_)),asserta(depth(DepthPrevious)),%(usual book-keeping)

	% When we have checked a whole set of successor nodes (for ex 'e,f,g' as successors of 'a'         % in the Russel basic example), we have to remove the last Beta value that we
	% used in our book-keeping for the Beta value
	(
	 isMax(Player),!,
	   retract(beta(_,_)) %should be the last written one
	   ; % idem if Player is min
	   retract(alpha(_,_))
	).


% recursive case:
value_list([H|NodeListRest], ValueListPartial, ValueList, Player):-
	depth(Depth),

	(
	isMax(Player),!,
	%******IF the Player is 'max'...

	    value(H,Value,_, Player),% Read the value of node 'H'.

	    alpha(Alpha,_),!,beta(Beta,_),!,%read the most local alpha and beta values

	    (Value =< Alpha,% *** IF... : this is the condition for Pruning:
	                     %if this 'Value' (= Value of one of the 'max' nodes, i.e a 'min' choice) is smaller than the best move for 'max' so far, it means that there's no chance 'max' ever chooses it  -->  prune, i.e. don't process further
	       % so nothing must happen, just return the 'Value',
	       % und update the Depth:
		write(Value), writeln('is smaller than alpha, thus I prune its subtree!'),
	        assignList([Value|[]],ValueList),
	     	DepthPrevious is Depth-1, retract(depth(_)),asserta(depth(DepthPrevious)),!

	      ; % *** ELSE : not a pruning case -->  process further :
	         min(Beta,Value,B_Temp), % update (if necessary) Beta
        	 ignore(retract(beta(_,Depth))),asserta(beta(B_Temp,Depth)),%retract the last one, and write new Beta (possibly the same)
        	 value_list(NodeListRest, [Value|ValueListPartial],ValueList, Player))%Process rest of list

	; %******ELSE(the Player is 'min' (all the following is fully analogical to the 'max' case)
	value(H,Value,_, Player),
	 alpha(Alpha,_),!,beta(Beta,_),!,
	(Value > Beta, %Pruning condition --> nothing happens,just note 'Value' und update Depth
	assignList([Value|[]],ValueList),
        DepthPrevious is Depth-1, retract(depth(_)),asserta(depth(DepthPrevious)),!
	; % (not a pruning case --> process further :)
	max(Alpha,Value,A_Temp),
	ignore(retract(alpha(_,Depth))),asserta(alpha(A_Temp,Depth)),
	value_list(NodeListRest, [Value|ValueListPartial],ValueList, Player)
	)
	).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SECTION for utility procedures for GDL
%% structures:                            %%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% putbrackets(+List, -BracketList) %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand(Player, State, Successors) :-
  findall(Player:Move1, gdl_legal(Player, Move1, State), MoveAlt),
  putbrackets(MoveAlt, MoveAltBrack),
  % pairs of Moves and NextStates for given State.
  write('In Expand, MoveAlt: '), writeln(MoveAlt),
  maplist(ggp_next_state(State), MoveAltBrack, Successors),
  write('In Expand, Successors: '), writeln(Successors:MoveAltBrack).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% putbrackets(+List, -BracketList) %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
putbrackets([], []).
putbrackets([Move|R1], [[Move]|R2]) :-
  putbrackets(R1, R2).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SECTION for simple utility procedures :%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max(X,Y,Max):-
	X>=Y,!,Max=X
	;
	Max=Y.

min(X,Y,Min):-
	X=<Y,!,Min=X
	;
	Min=Y.


% maxList (but we use the built-in predicate)
maxList([X],X).
maxList([X,Y|ListRest], Max):-
	maxList([Y|ListRest], MaxRest),
	max(X,MaxRest,Max).
minList([X],X).
minList([X,Y|ListRest], Min):-
	minList([Y|ListRest], MinRest),
	min(X,MinRest,Min).


% just a Unification
assignList(L,L).





% an equivalent implementation of the built-in reverse
reverseMy(L,Reversed):-
	reverseMy(L,[],Reversed).

reverseMy([],Acc,Acc).

reverseMy([H|Rest], Acc, Reversed):-
	reverseMy(Rest,[H|Acc], Reversed).




