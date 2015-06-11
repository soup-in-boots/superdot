-module(superdot).
-vsn("0.1.0").
-export([graph/1, graph/2, to_dot/1, pretty_name/1]).
-define(MAX_CHILDREN, 20).

pretty_name(Name) ->
    Name1 = unicode:characters_to_binary(io_lib:format("~p", [Name])),
    Name2 = re:replace(Name1, "[\\{\\}\\[\\]\"'/\\\\<>]", "", [unicode, global]),
    re:replace(Name2, "[,.-]", "_", [unicode, global, {return, binary}]).

to_dot({graph, Name, Type, Children}) ->
    PrettyName = pretty_name(Name),
    Color = case Type of
        simple -> "#ccccff";
        _ -> "#ffcccc"
    end,
    Output = [
        "digraph ", PrettyName, "{\n",
        PrettyName, " [shape=box,style=filled,fillcolor=\"",Color,"\"];\n",
        lists:map(fun
                ({node, ChildName}) ->
                    PrettyChildName = pretty_name(ChildName),
                    [
                        PrettyChildName,";\n",
                        PrettyName, " -> ", PrettyChildName, ";\n"
                    ];
                (SuperChild = {graph, ChildName, _Type, _Children}) ->
                    PrettyChildName = pretty_name(ChildName),
                    [
                        to_sub_dot(SuperChild),
                        PrettyName, " -> ", PrettyChildName, ";\n"
                    ]
            end, Children),
        "}\n"
    ],
    unicode:characters_to_binary(Output).

to_sub_dot({graph, Name, Type, Children}) ->
    PrettyName = pretty_name(Name),
    Color = case Type of
        simple -> "#ccccff";
        _ -> "#ffcccc"
    end,
    [
        "subgraph ", PrettyName, "{\n",
        PrettyName, " [shape=box,style=filled,fillcolor=\"",Color,"\"];\n",
        lists:map(fun
                ({node, ChildName}) ->
                    PrettyChildName = pretty_name(ChildName),
                    [
                        PrettyChildName,";\n",
                        PrettyName, " -> ", PrettyChildName, ";\n"
                    ];
                (SuperChild = {graph, ChildName, _Type, _Children}) ->
                    PrettyChildName = pretty_name(ChildName),
                    [
                        to_sub_dot(SuperChild),
                        PrettyName, " -> ", PrettyChildName, ";\n"
                    ]
            end, Children),
        "}\n"
    ].

graph(undefined) ->
    error(badarg);
graph(Sup) when is_atom(Sup) ->
    graph(whereis(Sup));
graph(Sup) when is_pid(Sup) ->
    {Name, Names} = get_name(Sup, dict:new()),
    element(1, graph(Name, Sup, Names)).


graph(Name, Sup) ->
    element(1, graph(Name, Sup, dict:new())).

graph(Name, Sup, Names) ->
    case supervisor:which_children(Sup) of
        [] ->
            {ChildName, Names2} = assert_name("unknown", Names),
            {{graph, Name, simple, [{node, ChildName}]}, Names2};
        [{undefined, Pid, supervisor, _Modules}|_Rest] ->
            {ChildName, Names2} = get_simple_name(Sup, Pid, Names),
            {Res, Names3} = graph(ChildName, Pid, Names2),
            {{graph, Name, simple, [Res]}, Names3};
        [{undefined, Pid, worker, _Modules}|_Rest] ->
            {ChildName, Names2} = get_simple_name(Sup, Pid, Names),
            {{graph, Name, simple, [{node, ChildName}]}, Names2};
        Children ->
            {GraphChildren, Names2} = lists:mapfoldl(fun
                    ({ChildName, _Pid, worker, _Modules}, NamesAcc) ->
                        {ChildName2, NamesAcc2} = assert_name(ChildName, NamesAcc),
                        {{node, ChildName2}, NamesAcc2};
                    ({ChildName, Pid, supervisor, _Modules}, NamesAcc) ->
                        {ChildName2, NamesAcc2} = assert_name(ChildName, NamesAcc),
                        graph(ChildName2, Pid, NamesAcc2)
                end, Names, Children),
            case length(GraphChildren) of
                N when N < ?MAX_CHILDREN ->
                    {{graph, Name, normal, GraphChildren}, Names2};
                N ->
                    {Taken, _} = lists:split(?MAX_CHILDREN, GraphChildren),
                    {{graph, Name, normal, Taken}, Names2}
            end
    end.

get_name(Pid, Names) ->
    Name = case process_info(Pid, registered_name) of
        [] ->
            {initial_call, {Mod, _, _}} = process_info(Pid, initial_call),
            Mod;
        Registered ->
            Registered
    end,
    Names2 = dict:update_counter(Name, 1, Names),
    case dict:fetch(Name, Names2) of
        1 -> {Name, Names2};
        N -> {{Name, N}, Names2}
    end.


assert_name(Name, Names) ->
    Names2 = dict:update_counter(Name, 1, Names),
    case dict:fetch(Name, Names2) of
        1 -> {Name, Names2};
        N -> {{Name, N}, Names2}
    end.

get_simple_name(Sup, Pid, Names) ->
    try
        %% First we try to get the state of the presumed supervisor
        %% We assert that its first element must be the atom state,
        %% as this is true for all supervisors using the supervisor
        %% behaviour.
        State       = sys:get_state(Sup),
        state       = element(1, State),

        %% Next we try to look up the child specifications.
        %% Here we assert that the first element of the presumed
        %% child specification must be the atom child, as this is
        %% true for all supervisors using the supervisor behaviour.
        ChildSpecs  = element(4, State),
        ChildDef    = hd(ChildSpecs),
        child       = element(1, ChildDef),

        %% Finally, in the child record, the third element is the
        %% name as specified in the child specification.
        %% Why they leave this out of the which_children response
        %% baffles me.
        Name        = element(3, ChildDef),
        assert_name(Name, Names)
    catch
        _:_ ->
            %% Assuming our assertions above failed, or some other
            %% unlikely error came up, fall back to the usual method
            get_name(Pid, Names)
    end.
