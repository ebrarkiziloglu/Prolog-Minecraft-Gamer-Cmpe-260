% asude ebrar kiziloglu
% 2019400009
% compiling: no
% complete: no
:- ['cmpecraft.pro'].
:- init_from_map.
% :- module(temp_dict, []).

% Predicate 3.1:
% the x-coordinates and y-coordinates are substracted and their absolute values are added:
manhattan_distance([X,Y], [Z,T], Distance) :- 
    N is X-Z, M is Y-T, Distance is abs(N) + abs(M).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Predicate 3.2:
% If the list has only 1 element, it is the result:
minimum_of_list([H], H).
% Otherwise the elements are compared one-by-one starting from the end of the list:
minimum_of_list([H|T], Minimum) :- 
    minimum_of_list(T, Min), Minimum is min(H, Min).

% Predicate 3.3:
% 10 points
% find_nearest_type([_, ObjectDict, _], ObjectType, -ObjKey, -Object, -Distance) :- 
min_of_dict(_{}, 16777216) :- !.
min_of_dict(Dict, MinValue) :-
    get_dict(K, Dict, V),
    del_dict(K, Dict, V, NewDict),
    min_of_dict(NewDict, MinValue_), MinValue is min(V, MinValue_).

find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :-
    State = [AgentDict, ObjectDict, _], 
    TempObjectDict = _{},
    findall(Object, ObjectDict.Object.type=ObjectType, KeyList),
    find_min(ObjectDict, KeyList, AgentDict.x, AgentDict.y, TempObjectDict, ResultDict),
    min_of_dict(ResultDict, Distance),
    get_dict(ObjKey, ResultDict, Distance),
    get_dict(ObjKey, ObjectDict, Object).

member_of_list(X, [X|_]) :- !.
member_of_list(X, [_|T]) :-
    member_of_list(X, T).

find_min(_, [], _, _, TempObjectDict, ResultDict) :- ResultDict = TempObjectDict.
find_min(Objects, KeyList, AgentX, AgentY, TempObjectDict, ResultDict) :-
    member_of_list(Key, KeyList), 
    get_dict(Key, Objects, Value),
    manhattan_distance([Value.x,Value.y], [AgentX, AgentY], Distance),
    put_dict(Key, TempObjectDict, Distance, NewDict),
    delete(KeyList, Key, KeyList_),
    find_min(Objects, KeyList_, AgentX, AgentY, NewDict, ResultDict).

% Predicate 3.4:
% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
% Given the difference of X and Y, and DepthLimit, the list is filled with actions:
% fill_action_list([], 0, 0, _) :- !.
% fill_action_list([go_right|NewActionList], X, Y, DepthLimit) :-
%     DepthLimit >= X+Y, X > 0, DX is X-1, DDepth is DepthLimit-1, fill_action_list(NewActionList, DX, Y, DDepth).
% fill_action_list([go_down|NewActionList], X, Y, DepthLimit) :-
%     DepthLimit >= X+Y, Y > 0, DY is Y-1, DDepth is DepthLimit-1, fill_action_list(NewActionList, X, DY, DDepth).
% Given X and Y coordinates, the difference of X and Y are calculated from agent's current location to the given location.
% Then, with the necessary parameters, fill_action_list predicate is called (defined above).
navigate_to(State, X, Y, ActionList, DepthLimit) :- 
    State = [AgentDict, _, _], 
    Diff_X is (X-AgentDict.x),
    Diff_Y is (Y-AgentDict.y),
    Diff_X < 0, Diff_Y < 0, Diff_X_ is -Diff_X, Diff_Y_ is -Diff_Y, 
    Totalmove = Diff_X_ + Diff_Y_ , 
    DepthLimit >= Totalmove ,
    fill_list_multiple_times(Horizontal_action, Diff_X_, 'go_left'),
    fill_list_multiple_times(Vertical_action, Diff_Y_, 'go_up'),
    append(Horizontal_action, Vertical_action, ActionList);

    State = [AgentDict, _, _], 
    Diff_X is (X-AgentDict.x),
    Diff_Y is (Y-AgentDict.y),
    Diff_X < 0, Diff_Y >= 0, Diff_X_ is -Diff_X,
    Totalmove is Diff_X_ + Diff_Y, 
    DepthLimit >= Totalmove ,
    fill_list_multiple_times(Horizontal_action, Diff_X_, 'go_left'),
    fill_list_multiple_times(Vertical_action, Diff_Y, 'go_down'),
    append(Horizontal_action, Vertical_action, ActionList);

    State = [AgentDict, _, _], 
    Diff_X is (X-AgentDict.x),
    Diff_Y is (Y-AgentDict.y),
    Diff_X >= 0, Diff_Y < 0, Diff_Y_ is -Diff_Y, 
    Totalmove is Diff_X + Diff_Y_,
    DepthLimit >= Totalmove ,
    fill_list_multiple_times(Horizontal_action, Diff_X, 'go_right'),
    fill_list_multiple_times(Vertical_action, Diff_Y_, 'go_up'),
    append(Horizontal_action, Vertical_action, ActionList);

    State = [AgentDict, _, _], 
    Diff_X is (X-AgentDict.x),
    Diff_Y is (Y-AgentDict.y),
    Diff_X >= 0, Diff_Y >= 0,
    Totalmove is Diff_X + Diff_Y,
    DepthLimit >= Totalmove ,
    fill_list_multiple_times(Horizontal_action, Diff_X, 'go_right'),
    fill_list_multiple_times(Vertical_action, Diff_Y, 'go_down'),
    append(Horizontal_action, Vertical_action, ActionList).

fill_list_multiple_times([], 0, _) :- !.
fill_list_multiple_times([Element|SubList], Number, Element) :- 
    Number_ is Number-1, 
    fill_list_multiple_times(SubList, Number_, Element).


% Predicate 3.5:
% chop_nearest_tree(+State, -ActionList) :- .
chop_nearest_tree(State, ActionList) :- 
    State = [AgentDict, _, _], 
    find_nearest_type(State, tree, TreeKey, Tree, Distance),
    get_dict(x, Tree, Tx), get_dict(y, Tree, Ty), 
    navigate_to(State, Tx, Ty, MoveList, Distance),
    fill_list_multiple_times(ChopList, 4, 'left_click_c'),
    append(MoveList, ChopList, ActionList).

% Predicate 3.6:
% mine_nearest_stone(+State, -ActionList) :- .
mine_nearest_stone(State, ActionList) :- 
    find_nearest_type(State, stone, StoneKey, Stone, Distance),
    get_dict(x, Stone, Sx), get_dict(y, Stone, Sy), 
    navigate_to(State, Sx, Sy, MoveList, Distance),
    fill_list_multiple_times(MineList, 4, 'left_click_c'),
    append(MoveList, MineList, ActionList).

% Predicate 3.7:
% gather_nearest_food(+State, -ActionList) :- .
gather_nearest_food(State, ActionList) :- 
    find_nearest_type(State, food, FoodKey, Food, Distance),
    get_dict(x, Food, Fx), get_dict(y, Food, Fy), 
    navigate_to(State, Fx, Fy, MoveList, Distance),
    fill_list_multiple_times(MineList, 1, 'left_click_c'),
    append(MoveList, MineList, ActionList).


% Predicate 3.8:
% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .


% Predicate 3.9:
% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .


% Predicate 3.10:
% 15 points
% make_castle(+State, -ActionList) :- .

