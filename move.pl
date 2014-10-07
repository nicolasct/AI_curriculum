% A Min-Max algorithm with alpha-beta pruning for playing 
% in the General Game Player environment for
% Methods of AI 2009




% initialize
ggp_terminator_player([], Role, _Move, _Time, _MovesList) :-
   assert(alpha(-9999,0)), assert(beta(9999,0)),
   assert(depth(0)),
   asserta(isMax(Role)),
     % determine opponent
       findall(OneRole, gdl_role(OneRole), Roles),
       select(Role, Roles, OpponentRoles),
       member(Opponent, OpponentRoles),
       asserta(isMin(Opponent)),

   writeln('$$$$$$$$ Initialization succeeded! $$$$$$$$$').

% determine a move
ggp_terminator_player(State, Role, Move, _Time, _MovesList) :-
  ggp_store(Role, db_flag(0)),
  value(State, _Value, _Index, -9999, 9999, Role, start),
  findall(Role:M, gdl_legal(Role,M,State), MoveAlt),
  ggp_db(1, 'legal moves':Role:MoveAlt),
  length(MoveAlt, L),
  N is random(L),
  nth0(N, MoveAlt, Move).
  
  
  
  
  %value(Node, Value, Alpha, Beta):-integer(Node),Value=Node.
%otherwise, the value of a node is the max_value (bzw min_value)
%   that the max player (bzw min player) should choose from this node:
% Alpha and Beta are lists, b/c I have no clue otherwise how
%to reassign a variable in Prolog!

value(Node,Value,Index,Alpha,Beta,Player, start):-
	retractall(alpha(_,_)), retractall(beta(_,_)),
	asserta(alpha(Alpha,0)),asserta(alpha(Alpha,0)), asserta(beta(Beta,0)),asserta(beta(Beta,0)),%man schreibt das 2 mal, weil man später immer retract/assert macht, aber das erste Mal würde schon nichts mehr bleiben, wenn man nicht 2 mal schreiben würde
	retractall(depth(_)),asserta(depth(0)),

    write('Test: '), writeln(Node:Value:Index:Alpha:Beta:Player), 

	value(Node, Value,Index, Player).

value(Node, Value, Index, Player):-
    write('in value() fkt 1: '), writeln(Node:Value:Index:Player), % debugging
	isMax(Player),!,max_value(Node,Value,Index, Player)
	;
    write('in value() fkt 2: '), writeln(Node:Value:Index:Player), % debugging
	isMin(Player),!,min_value(Node, Value,Index, Player).
%%%%%%% END OF VALUE OF A NODE %%%%%

% returns the max value of the children of 'Node':
max_value(Node, Value,Index, Player):-

    write('in max_value() fkt: '), writeln(Node:Value:Index:Player), % debugging

	%if we can read directly the Value (terminal state), then simply read it:
 %	value(Node, Value, Alpha_Init, Beta_Init, Alpha_New, Beta_New),
 
  	gdl_terminal(Node), % check whether it is a terminal state
	gdl_goal(Player, Value, Node), ! ,% get the value of the goal state
        write('Value of Goalstate: '), write(Value), write(' for player '), writeln(Player) % debugging

%debil	depth(X), retract(depth(_)),Y is X-1, asserta(depth(Y)),!
	%otherwise, we have to expand the Node, and take the max value of the children:
	;
	(   expand(Node,Successors),




	depth(X),Y is X+1,retract(depth(_)), asserta(depth(Y)),
	isMin(Min),
	value_list(Successors,[],Values,  Min),
	max_list(Values, Value)),
	nth1(Index,Values,Value).
	%(   Value >= Beta_Init,!
%	;
%	max(Alpha_Init,Value,Alpha_New))).


% returns the min value of the children of 'Node':
min_value(Node, Value,Index, Player):-

    write('in min_value() fkt: '), writeln(Node:Value:Index:Player), % debugging

	%if we can read directly the Value (terminal state), then simply read it:
	%value(Node, Value, Alpha_Init, Beta_Init, Alpha_New, Beta_New),
	
  	gdl_terminal(Node), % check whether it is a terminal state
	gdl_goal(Player, Value, Node), ! ,% get the value of the goal state
        write('Value of Goalstate: '), write(Value), write(' for player '), writeln(Player) % debugging

%debile	depth(X), retract(depth(_)),Y is X-1, asserta(depth(Y)),

	%otherwise, we have to expand the Node, and take the max value of the children:
	;
	( expand(Node,Successors),
	depth(X), retract(depth(_)), Y is X+1,asserta(depth(Y)),
	isMax(Max),
	value_list(Successors,[],Values, Max),
	min_list(Values, Value)),
	nth1(Index,Values,Value).
	%(   Value =< Alpha_Init,!
%	;
%	min(Beta_Init,Value,Beta_New))).




% utility comparator function : Max = "max"?
% isMax(max).
% isMin(min).

%min_value(Value,State):- terminal(State), value(State, Value).

%min_value(Value,State):-
%	expand(State, Successors),

%	max_value(Value,Succ).

%max_value(Value,State):- terminal(State), value(State, Value).

%max_value(Value,State):- s(State, Succ), min_value(Value,Succ).
%

% The old version of min_value (the one I've done in Action&Cognition)
%min_value(N, Value):-
%	expand(N,Successors),
%	value_list(Successors,[], Values),
%	max_list(Values, Value).


%%%%%%%%%%%%%%%%%%%
% Read the values of a list of nodes
% Alpha and Beta are lists
%Stop case:
value_list([],ValueListPartial,ValueList, Player):-
	reverseMy(ValueListPartial, ValueList),
	depth(Depth),DepthPrevious is Depth-1, retract(depth(_)),asserta(depth(DepthPrevious)),
	%when for ex, Player is max, we have checked a whole set (for ex
	% e,f,g) of max nodes, so we have to remove the beta value that we
	% wrote for the exploration of these possible choices for min)
	(
	 isMax(Player),!,
	   retract(beta(_,_))%should be the last written one
	   ; % idem if Player is min
	   retract(alpha(_,_))
	).




value_list([H|NodeListRest], ValueListPartial, ValueList, Player):-
	depth(Depth),
	(
	isMax(Player),!,%if the Player is 'max'
	    value(H,Value,_, Player),
	    alpha(Alpha,_),!,beta(Beta,_),!,%take only the first
	    (Value =< Alpha,% if this Value (= Value of one of the 'max' nodes, i.e a 'min' choice) is smaller than the best move for 'max' so far, it means that even if 'min' choose it, there's no chance 'max' chooses it one level higher -->  prune, i.e. don't process further
	       %then nothing must happen, merk dir nur das Wert 'Value',
	       % und update the Depth
	      
		write(Value), writeln('is small than alpha, thus I prune its subtree!'),

	       assignList([Value|[]],ValueList),
	     	DepthPrevious is Depth-1, retract(depth(_)),asserta(depth(DepthPrevious)),!
	      % min(Beta,Value,B_Temp), The branche is dead : don't update
	      % retractall(beta(_)), assert(beta(B_Temp)),!
	      ; % else update Beta and process further
	        % max(Alpha_Init,Value,Alpha_NewTemp),
	         min(Beta,Value,B_Temp),
        	 ignore(retract(beta(_,Depth))),asserta(beta(B_Temp,Depth)),%retract the last one, and write new one
        	 value_list(NodeListRest, [Value|ValueListPartial],ValueList, Player))
	;% else if the Player is 'min' (analogical)
	value(H,Value,_, Player),%I need to repeat that, otherwise it doesn't see the Wert of Value anymore in the 'else'
	%retract
	 alpha(Alpha,_),!,beta(Beta,_),!,%idem, this should be repeated b/c of the OR
	(Value > Beta, %nothing happens,just merk dir die Wert und update Depth
	assignList([Value|[]],ValueList),
	       DepthPrevious is Depth-1, retract(depth(_)),asserta(depth(DepthPrevious)),!
	%max(Alpha,Value,A_Temp), the branch is dead, don't update Alpha!
	% retractall(alpha(_)), assert(alpha(A_Temp)),!
	;
	max(Alpha,Value,A_Temp),
	ignore(retract(alpha(_,Depth))),asserta(alpha(A_Temp,Depth)),
	value_list(NodeListRest, [Value|ValueListPartial],ValueList, Player)
	)
	).



max(X,Y,Max):-
	X>=Y,!,Max=X
	;
	Max=Y.

min(X,Y,Min):-
	X=<Y,!,Min=X
	;
	Min=Y.


maxList([X],X).
maxList([X,Y|ListRest], Max):-
	maxList([Y|ListRest], MaxRest),
	max(X,MaxRest,Max).
minList([X],X).
minList([X,Y|ListRest], Min):-
	minList([Y|ListRest], MinRest),
	min(X,MinRest,Min).


% (not used any more)
%convert(N,Value):-
	%value(N,Value).


%expand(State, Successors):- findall(Successor,
%				  s(State,Successor),
%				  Successors).

assignList(L,L).

%breadth_first(To_do,Path_up_to_know)
%breadth_first([H|_To_do], PathAccum, PathAccum ):- terminal(H).


%breadth_first([H|To_do], Path, PathAccum):-
%	expand(H, Children),
%	append(To_do,Children,NewAgenda),
%	append(Path,Children,NewPath),
%	breadth_first( NewAgenda, NewPath,PathAccum).
%
%
%breadth_first  INITIAL CALL
%
b_f(Start, Path):-
	b_f(PartialPath,Path,[Start]).

%Stop case
b_f(CompletePath,CompletePath,[]).

b_f(PartialPath, Path, [H|AgendaRest]):-
	expand(H,Expansion),
	append(AgendaRest,Expansion, NewAgenda),
	b_f([H|PartialPath], Path, NewAgenda).



%Experimenting with accumulators
%The naive Reverse:
%reverse([],[]).

%reverse([H|Rest], Reversed):-
	%reverse(Rest, Rvrsd),
%	append(Rvrsd, [H], Reversed).

append([],L1,L1).

append([H|LRest], L1, [H|L2]):-
	append(LRest, L1, L2).

sumList([X],X).

sumList([H|Rest],Sum):-
	sumList(Rest,X),
	Sum is H + X.

sumListTail([],Sum,Sum).

sumListTail([H|Rest],Partial,Sum):-
	Partial2 is Partial + H,
	sumListTail(Rest,Partial2, Sum).

reverseMy(L,Reversed):-
	reverseMy(L,[],Reversed).

reverseMy([],Acc,Acc).

reverseMy([H|Rest], Acc, Reversed):-
	reverseMy(Rest,[H|Acc], Reversed).

concat(L1-L2, L2, L1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand(State, Successors) :-                                           
  findall(Player:Move1, gdl_legal(Player, Move1, State), MoveAlt),                      
  putbrackets(MoveAlt, MoveAltBrack),                                               
  % pairs of Moves and NextStates for given State.                                  
  maplist(ggp_next_state(State), MoveAltBrack, Successors).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% putbrackets(+List, -BracketList) %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
putbrackets([], []).
putbrackets([Move|R1], [[Move]|R2]) :-
  putbrackets(R1, R2).

