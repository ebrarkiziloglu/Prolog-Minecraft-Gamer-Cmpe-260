% asude ebrar kiziloglu
% 2019400009
% compiling: yes
% complete: yes
:- ['cmpecraft.pro'].
:- init_from_map.

% Predicate 3.1:
% the x-coordinates and y-coordinates are substracted and their absolute values are added:
manhattan_distance(A, B, Distance) :- 
    A = [X, Y], B = [Z, T],
    N is X-Z, M is Y-T, Distance is abs(N) + abs(M).

% Predicate 3.2: Following predicate finds the minimum element in a list:
% If the list has only 1 element, it is the result:
minimum_of_list([H], H).
minimum_of_list([H|T], Minimum) :- 
    minimum_of_list(T, Min), Minimum is min(H, Min).

% Predicate 3.3:
% In this predicate, firstly, all of the keys of the objects with the given type is gathered in the list KeyList.
% Then, this list is sent to find_distances predicate to determine the distances.
% Object keys and their distances to the agent are now recorded in the dictionary ResultDict.
% This dictionary is sent to min_of_dict predicate to determine the minimum value (aka min distance).
% Then, with 2 get_dict's, the key of this min distance and and the corresponding object is determined.
find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :-
    State = [AgentDict, ObjectDict, _], 
    TempObjectDict = _{},
    findall(Object, ObjectDict.Object.type=ObjectType, KeyList),
    find_distances(ObjectDict, KeyList, AgentDict.x, AgentDict.y, TempObjectDict, ResultDict),
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
find_distances(_, [], _, _, TempObjectDict, ResultDict) :- ResultDict = TempObjectDict.
find_distances(Objects, KeyList, AgentX, AgentY, TempObjectDict, ResultDict) :-
    member(Key, KeyList), 
    get_dict(Key, Objects, Value),
    manhattan_distance([Value.x,Value.y], [AgentX, AgentY], Distance),
    put_dict(Key, TempObjectDict, Distance, NewDict),
    delete(KeyList, Key, KeyList_),
    find_distances(Objects, KeyList_, AgentX, AgentY, NewDict, ResultDict).

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
    ;   Diff_X < 0, Diff_Y >= 0, Diff_X_ is -Diff_X,
        Totalmove is Diff_X_ + Diff_Y, 
        DepthLimit >= Totalmove ,
        fill_list_multiple_times(Horizontal_action, Diff_X_, 'go_left'),
        fill_list_multiple_times(Vertical_action, Diff_Y, 'go_down')
    ;   Diff_X >= 0, Diff_Y < 0, Diff_Y_ is -Diff_Y, 
        Totalmove is Diff_X + Diff_Y_,
        DepthLimit >= Totalmove ,
        fill_list_multiple_times(Horizontal_action, Diff_X, 'go_right'),
        fill_list_multiple_times(Vertical_action, Diff_Y_, 'go_up')
    ;   Diff_X >= 0, Diff_Y >= 0,
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

% Predicate 3.5 - 3.6 - 3.7:
% These 4 predicates are combined in a common predicate acquire_nearest_object :
chop_nearest_tree(State, ActionList) :- acquire_nearest_object(State, tree, ActionList).
mine_nearest_stone(State, ActionList) :- acquire_nearest_object(State, stone, ActionList).
gather_nearest_food(State, ActionList) :- acquire_nearest_object(State, food, ActionList).
mine_nearest_cobblestone(State, ActionList) :- acquire_nearest_object(State, cobblestone, ActionList).

% This combined predicate firstly determine the location of the nearest object.
% Then navigate_to that location, and ceate an action list for the left clicking necessary number of times. 
% And append this list to the list coming from navigate_to to form ActionList together.
acquire_nearest_object(State, Type, ActionList) :-
    % tree / stone / food
    find_nearest_type(State, Type, _, Object, Distance),
    get_dict(x, Object, Ox), get_dict(y, Object, Oy),
    navigate_to(State, Ox, Oy, MoveList, Distance),
    (
        Type = food, fill_list_multiple_times(ClickList, 1, 'left_click_c')
    ;   Type \= food, fill_list_multiple_times(ClickList, 4, 'left_click_c')
    ),
    append(MoveList, ClickList, ActionList).

% Predicate 3.8:
% Following predicate generates an action list to make necessary actions to be able to craft an item with the given Type.
% According to Type, one of the generate_requirement_list predicates is called (2 predicates defined below).
% With the required items list now avaliable, generate_actions predicate is called to collect all of the required items.
% List of actions coming from generate_actions and RequiredCraftList (either empty or has only element 'craft_stick') are appended to form ActionList.
collect_requirements(State, Type, ActionList) :-
    State = [AgentDict, _, _],
    (
        Type = stick, 
        generate_requirement_list_for_stick(AgentDict, ReqList),
        RequiredCraftList = []
        
        ; Type = stone_pickaxe,  
        generate_requirement_list_for_axes(AgentDict, ReqList, NeedStickCraft),
        (
            NeedStickCraft, RequiredCraftList = [craft_stick];
            \+NeedStickCraft, RequiredCraftList = []
        )
        ; Type = stone_axe, 
        generate_requirement_list_for_axes(AgentDict, ReqList, NeedStickCraft),
        (
            NeedStickCraft, RequiredCraftList = [craft_stick];
            \+NeedStickCraft, RequiredCraftList = []
        )
    ),
    generate_actions(State, ReqList, InitialActions),
    append(InitialActions, RequiredCraftList, ActionList).

% Following predicate creates the necessary items list for a stick to be crafted, 
% according to the current number of sticks in the inventory.
% This predicate is called in the collect_requirements predicate.
generate_requirement_list_for_stick(AgentDict, ReqList) :-
    get_dict(inventory, AgentDict, Inv),
    (
        LogCount = Inv.get(log), 
        (
            LogCount > 1, !, ReqList = []
        ;   LogCount < 2, !, ReqList = [tree]
        )
    ;   \+Inv.get(log), !, ReqList = [tree]
    ).

% Following predicate creates the necessary items list for a stone_pickaxe or stone_axe to be crafted, 
% according to the current number of sticks, logs and cobblestones in the inventory.
% Parameter NeedStickCraft keeps the knowledge of whether a stick is needed to be crafted or not.
% This information becomes handy in the predicate collect_requirements.
% This predicate is called in the collect_requirements predicate.
generate_requirement_list_for_axes(AgentDict, ReqList, NeedStickCraft) :-
    get_dict(inventory, AgentDict, Inv),

    % Inventory check for logs:
    (
        LogCount = Inv.get(log), 
        (
            LogCount > 4 -> % enough logs for both log and stick requirement,
            LogList = [], !, TreeForStick = false;
            LogCount > 2 -> % not enough logs for stick requirement,
            LogList = [/*tree_for_stick*/], !, TreeForStick = true;
            LogCount = 2 -> LogList = [tree], !, TreeForStick = false;
            LogCount < 2 -> % 1 tree needed for log requirement, 
            % 1 tree may be needed for stick requirement
            LogList = [tree/*, tree_for_stick*/], !, TreeForStick = true
        );
        \+Inv.get(log), 
        % 1 tree needed for log requirement, 1 tree may be needed for stick requirement
        LogList = [tree/*, tree_for_stick*/], !, TreeForStick = true
    ),

    % Inventory check for 2 sticks:
    (
        StickCount = Inv.get(stick), 
        (
            StickCount > 1, StickList = [], NeedStickCraft = false
        ;   StickCount < 2, % stick needed
            (
                TreeForStick, StickList = [tree];
                \+TreeForStick, StickList = []
            ), NeedStickCraft = true  
        );   

        \+ Inv.get(stick), % stick needed
        (
            TreeForStick, StickList = [tree];
            \+TreeForStick, StickList = []
        ), NeedStickCraft = true  
    ),

    % Inventory check for 3 cobblestones:
    (
        CobbleCount = Inv.get(cobblestone),
        (
            CobbleCount > 2, CobbleList = []
        ;   CobbleCount < 3, CobbleList = [stone]
        )

    ;   \+Inv.get(cobblestone), CobbleList = [stone]    
    ),

    append(CobbleList, LogList, InterList),
    append(InterList, StickList, ReqList).

% With the given list of items ItemList, this predicate select an item, 
% chop or mine it according to the type and keeps the data of the necessary actions.
% Then with recursion, for all of the items in the list, necessary actions aree appended together to form ActionList at the end.
% This predicate is called in the collect_requirements predicate.
generate_actions(_, [], []) :- !.
generate_actions(State, ItemList, ActionList) :-
    select(Elem, ItemList, NewItemList),
    (
        Elem = tree,
        chop_nearest_tree(State, ActionList1)
    ;   Elem = stone,
        mine_nearest_stone(State, ActionList1)
    ),
    execute_actions(State, ActionList1, NextState),
    generate_actions(NextState, NewItemList, ActionList2),
    append(ActionList1, ActionList2, ActionList).

% Predicate 3.9:
% Recursive predicate (defined below) is called to find an appropriate location for a castle.    
find_castle_location(State, XMin, YMin, XMax, YMax) :- 
    State = [_, ObjectDict, _],
    % width(W), height(H):
    % XMin = 1, 2, 3, ..., W-4
    % YMin = 1, 2, 3, ..., H-4
    find_castle(ObjectDict, 1, 1, XMin, YMin), 
    XMax is XMin+2, YMax is YMin+2.

% This predicate is called inside find_castle_location predicate with initial value for X=1 and Y=1
% The goal here is to find an appropriate location for a castle. It works as the RECURSION step in this goal. 
% If the current values of X and Y are appropriate, X and Y are returned.
% Otherwise, if we're not at the end of line, we move horizontally to right to check the new locations for a castle.
% If we're at the end of line, we move vertically to down. 
find_castle(Dict, X, Y, ResX, ResY) :-
    \+can_be_castle(Dict, X, Y), width(W), height(H), MaxW is W-4, MaxH is H-4,
    (
        X < MaxW, XNew is X+1, YNew is Y, find_castle(Dict, XNew, YNew, ResX, ResY);
        X = MaxW, 
        (
            Y = MaxH, !, false; % the down-right-most corner is reached without any success, false is returned
            Y < MaxH, YNew is Y+1, XNew is 1, find_castle(Dict, XNew, YNew, ResX, ResY)
        )
    );
    can_be_castle(Dict, X, Y), !, ResX is X, ResY is Y.

% This predicate checks whether the given XMin and YMin coordinates can be a start point for a castle (a.k.a. upper-left-most-point),
% by checking for availability of all of the 9 possible tiles.
can_be_castle(Dict, XMin, YMin) :-
    X1 is XMin, X2 is XMin+1, X3 is XMin+2,
    Y1 is YMin, Y2 is YMin+1, Y3 is YMin+2,
    is_free(X1, Y1, Dict), is_free(X1, Y2, Dict), is_free(X1, Y3, Dict),
    is_free(X2, Y1, Dict), is_free(X2, Y2, Dict), is_free(X2, Y3, Dict),
    is_free(X3, Y1, Dict), is_free(X3, Y2, Dict), is_free(X3, Y3, Dict), !.

% Given coordinates, this predicate checks whether the tile is occupied or not.
is_free(X, Y, ObjectDict) :- \+is_occupied(X, Y, ObjectDict) /*write('FREE'), nl*/.
is_occupied(X, Y, ObjectDict) :-
    get_dict(_, ObjectDict, Object),
    get_dict(x, Object, Ox), get_dict(y, Object, Oy), 
    X = Ox, Y = Oy, !.

% Predicate 3.10:
% This predicate first generates an action list (CoubleActions) to reach 9 cobblestones in the directory
% Then, using the predicate find_castle_location, it searchs for a location for the castle.
% In case of success, first the actions to navigate to the start point of the castle (a.k.a. upper-left-most-point) are generated using navigate_to.
% Then, actions to build the castle are generated using build_castle.
% Finally all of the actions are combined to form ActionList.
make_castle(State, ActionList) :- 
    State = [AgentDict, _, _],
    get_dict(inventory, AgentDict, Inv),
    (
        CobbleCount = Inv.get(cobblestone),
        collect_cobblestones(State, CoubleActions, CobbleCount)
        ; \+Inv.get(cobblestone), collect_cobblestones(State, CoubleActions, 0)
    ),
    execute_actions(State, CoubleActions, InterState),
    find_castle_location(InterState, XMin, YMin, _, _),
    X1 is XMin, Y1 is YMin, 
    width(W), height(H), Depth is W+H,
    navigate_to(InterState, X1, Y1, InitialMoveList, Depth),
    append(CoubleActions, InitialMoveList, InitialList),
    build_castle(BuildList),
    append(InitialList, BuildList, ActionList).

% Following predicate generates actions to reach 9 cobblestones in the inventory
% Parameter Count here represents the current number of cobblestones in the inventory
% The priority here is to mine stones over cobblestones (since 1 stone yields more cobblestones).
collect_cobblestones(State, ActionList, Count) :-
    Count > 8, !, ActionList = [];

    Count < 9, 
    mine_nearest_stone(State, IntermediateActionList),
    NewCount is Count+3,
    execute_actions(State, IntermediateActionList, NewState),
    collect_cobblestones(NewState, Actions, NewCount),
    append(IntermediateActionList, Actions, ActionList);

    Count < 9, 
    mine_nearest_cobblestone(State, IntermediateActionList),
    NewCount is Count+1,
    execute_actions(State, IntermediateActionList, NewState),
    collect_cobblestones(NewState, Actions, NewCount),
    append(IntermediateActionList, Actions, ActionList).

% Following predicate only exists for readability. 
% It just generates the necessary action list to travel along the castle while placing cobblestones.
build_castle(ActionList) :-
    append([place_c, go_right, place_c, go_right, place_c, go_down, 
        place_c, go_left, place_c, go_left, place_c, go_down], [place_c, go_right, place_c, go_right, place_c], ActionList).
