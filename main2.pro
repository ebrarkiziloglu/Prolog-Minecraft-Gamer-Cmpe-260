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

% Predicate 3.2:
% If the list has only 1 element, it is the result:
minimum_of_list([H], H).
% Otherwise the elements are compared one-by-one starting from the end of the list:
minimum_of_list([H|T], Minimum) :- 
    minimum_of_list(T, Min), Minimum is min(H, Min).

% Predicate 3.3:
% In this predicate, firstly, all of the keys of the objects with the given type is gathered in the list KeyList.
% Then, this list is sent to find_min predicate to determine the distances.
% Object keys and their distances to the agent are now recorded in the dictionary ResultDict.
% This dictionary is sent to min_of_dict predicate to determine the minimum value (aka min distance).
% Then, with 2 get_dict's, the key of this min distance and and the corresponding object is determined.
find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :-
    State = [AgentDict, ObjectDict, _], 
    TempObjectDict = _{},
    findall(Object, ObjectDict.Object.type=ObjectType, KeyList),
    find_min(ObjectDict, KeyList, AgentDict.x, AgentDict.y, TempObjectDict, ResultDict),
    min_of_dict(ResultDict, Distance),
    get_dict(ObjKey, ResultDict, Distance),
    get_dict(ObjKey, ObjectDict, Object).

% Given a dictionary, following predicate finds the minimum value in the dictionary.
min_of_dict(_{}, 16777216) :- !.
min_of_dict(Dict, MinValue) :-
    get_dict(K, Dict, V),
    del_dict(K, Dict, V, NewDict),
    min_of_dict(NewDict, MinValue_), MinValue is min(V, MinValue_).

% This predicate takes a dictionary and a key list and for each of the keys in the list, 
% reaches the corresponding object in the Objects dictionary.
% For each of these objects, it calculates the manhattan_dictsance between the object and the agent,
% and the object key and the distance are added to the dictionary.
find_min(_, [], _, _, TempObjectDict, ResultDict) :- ResultDict = TempObjectDict.
find_min(Objects, KeyList, AgentX, AgentY, TempObjectDict, ResultDict) :-
    member(Key, KeyList), 
    % member_of_list(Key, KeyList), 
    get_dict(Key, Objects, Value),
    manhattan_distance([Value.x,Value.y], [AgentX, AgentY], Distance),
    put_dict(Key, TempObjectDict, Distance, NewDict),
    delete(KeyList, Key, KeyList_),
    find_min(Objects, KeyList_, AgentX, AgentY, NewDict, ResultDict).

% member_of_list(X, [X|_]) :- !.
% member_of_list(X, [_|T]) :-
%     member_of_list(X, T).

% Predicate 3.4:
% Given X and Y coordinates, the difference of X and Y are calculated from agent's current location 
% to the given location. With the negativity check, the target location's placement with respect to the agent is determined. 
% Target location might be in the left-up or left-down or right-up or right-down of the agent.
% According the this position, Horizontal and vertical action lists are filled and then they are appended to form ActionList.
navigate_to(State, X, Y, ActionList, DepthLimit) :- 
    State = [AgentDict, _, _], 
    Diff_X is (X-AgentDict.x),
    Diff_Y is (Y-AgentDict.y),
    (
        Diff_X < 0, Diff_Y < 0, Diff_X_ is -Diff_X, Diff_Y_ is -Diff_Y, 
        Totalmove = Diff_X_ + Diff_Y_ , 
        DepthLimit >= Totalmove ,
        fill_list_multiple_times(Horizontal_action, Diff_X_, 'go_left'),
        fill_list_multiple_times(Vertical_action, Diff_Y_, 'go_up')
        ; 
        Diff_X < 0, Diff_Y >= 0, Diff_X_ is -Diff_X,
        Totalmove is Diff_X_ + Diff_Y, 
        DepthLimit >= Totalmove ,
        fill_list_multiple_times(Horizontal_action, Diff_X_, 'go_left'),
        fill_list_multiple_times(Vertical_action, Diff_Y, 'go_down')
        ;
        Diff_X >= 0, Diff_Y < 0, Diff_Y_ is -Diff_Y, 
        Totalmove is Diff_X + Diff_Y_,
        DepthLimit >= Totalmove ,
        fill_list_multiple_times(Horizontal_action, Diff_X, 'go_right'),
        fill_list_multiple_times(Vertical_action, Diff_Y_, 'go_up')
        ;
        Diff_X >= 0, Diff_Y >= 0,
        Totalmove is Diff_X + Diff_Y,
        DepthLimit >= Totalmove ,
        fill_list_multiple_times(Horizontal_action, Diff_X, 'go_right'),
        fill_list_multiple_times(Vertical_action, Diff_Y, 'go_down')
        ),
    append(Horizontal_action, Vertical_action, ActionList).

% Given an element and a #number, a list is filled with this element the #number times.
fill_list_multiple_times([], 0, _) :- !.
fill_list_multiple_times([Element|SubList], Number, Element) :- 
    Number_ is Number-1, 
    fill_list_multiple_times(SubList, Number_, Element).

% Predicate 3.5:
% Following predicate firstly determine the location of the nearest tree, then navigate_to that location.
% And ceate an action list for the left clicking 4 times, 
% and append this list to the list coming from navigate_to to together form ActionList.
chop_nearest_tree(State, ActionList) :- 
    State = [AgentDict, _, _], 
    find_nearest_type(State, tree, _, Tree, Distance),
    get_dict(x, Tree, Tx), get_dict(y, Tree, Ty), 
    navigate_to(State, Tx, Ty, MoveList, Distance),
    fill_list_multiple_times(ChopList, 4, 'left_click_c'),
    append(MoveList, ChopList, ActionList).

% Predicate 3.6:
% Following predicate firstly determine the location of the nearest stone, then navigate_to that location.
% And ceate an action list for the left clicking 4 times, 
% and append this list to the list coming from navigate_to to together form ActionList.
mine_nearest_stone(State, ActionList) :- 
    find_nearest_type(State, stone, _, Stone, Distance),
    get_dict(x, Stone, Sx), get_dict(y, Stone, Sy), 
    navigate_to(State, Sx, Sy, MoveList, Distance),
    fill_list_multiple_times(MineList, 4, 'left_click_c'),
    append(MoveList, MineList, ActionList).

% Predicate 3.7:
% Following predicate firstly determine the location of the nearest food, then navigate_to that location.
% And ceate an action list for the left clicking 1 time, 
% and append this list to the list coming from navigate_to to together form ActionList.
gather_nearest_food(State, ActionList) :- 
    find_nearest_type(State, food, _, Food, Distance),
    get_dict(x, Food, Fx), get_dict(y, Food, Fy), 
    navigate_to(State, Fx, Fy, MoveList, Distance),
    fill_list_multiple_times(MineList, 1, 'left_click_c'),
    append(MoveList, MineList, ActionList).

% Predicate 3.8:
% collect_requirements(+State, +ItemType, -ActionList) :- . 
required_inventory(stick, [tree], 4, []).
required_inventory(Type, [tree, tree, stone], 100, RequiredCrafts) :-
    Type = stone_pickaxe, RequiredCrafts = [craft_stick]; 
    Type = stone_axe, RequiredCrafts = [craft_stick].

generate_actions(State, ItemList, ActionList) :-
    (
        ItemList = [], ActionList = []
        ;
        select(Elem, ItemList, NewItemList),
        (
            Elem = tree,
            chop_nearest_tree(State, ActionList1)
            ; Elem = stone,
            mine_nearest_stone(State, ActionList1)
        ),
        execute_actions(State, ActionList1, NextState),
        generate_actions(NextState, NewItemList, ActionList2),
        append(ActionList1, ActionList2, ActionList)
    ).

collect_requirements(State, Type, ActionList) :-
    required_inventory(Type, ItemList, Number, RequiredCraftList),
    generate_actions(State, ItemList, InitialActions),
    append(InitialActions, RequiredCraftList, ActionList).

% Predicate 3.9:
% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .

% This predicate will help the agent to find a location to build a castle. 
% Castle is just a fancy name for a three by three cobblestone blocks. 
% An appropriate location for a castle is a three by three area that does not contain any object. 
% If there is no appropriate location, then the predicate should be false. 
% The following example is valid for the given example map file.


% Predicate 3.10:
% 15 points
% make_castle(+State, -ActionList) :- .

