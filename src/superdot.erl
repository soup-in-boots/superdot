-module(superdot).
-vsn("0.1.0").
-export([graph/1, graph/2]).
-export([to_dot/1, to_dot/2, to_dot/3]).
-export([default_options/2, default_graph_options/0]).

-type node_type() :: 'simple_sup' | 'sup' | 'worker'.
-type node_name() :: binary().
-type node_text() :: binary().
-type node_color() :: {0..255, 0..255, 0..255} | nonempty_list() | binary().
%% TODO: Add all supported shapes
-type node_shape() :: 'box'|'ellipse'.
%% TODO: Add all per-node options
-type node_option() :: {'fillcolor', node_color()}|{'shape', node_shape()}|'filled'|{'filled', boolean()}|{'label',iodata()}.
-type node_options() :: [] | [node_option()] | fun((node_name(), node_type()) -> node_options()).
-type node_children() :: [] | [graph_node()].
-type graph_node() :: {'node', node_name(), node_text(), node_type(), node_children()}.

-define(MAX_CHILDREN, 20).

-spec pretty_name(term()) -> binary().
pretty_name(Name) ->
    Name1 = unicode:characters_to_binary(io_lib:format("~p", [Name])),
    Name2 = re:replace(Name1, "[\\{\\}\\[\\]\"'/\\\\<>]", "", [unicode, global]),
    re:replace(Name2, "[,.-]", "_", [unicode, global, {return, binary}]).

-spec default_options(node_name(), node_type()) -> node_options().
default_options(_Name, simple_sup) ->
    [
        filled,
        {fillcolor,     "#ccccff"},
        {shape,         box}
    ];
default_options(_Name, sup) ->
    [
        filled,
        {fillcolor,     "#ffcccc"},
        {shape,         box}
    ];
default_options(_Name, worker) ->
    [
    ].

default_graph_options() ->
    [
        {splines, ortho}
    ].

to_dot(Node) ->
    to_dot(Node, fun default_options/2, default_graph_options()).

to_dot(Node, Options) ->
    to_dot(Node, Options, default_graph_options()).

-spec to_dot(graph_node(), node_options()) -> binary().
to_dot({node, Name, Text, Type, Children}, Options, GraphOptions) ->
    Output = make_dot_tree("digraph", Name, Text, Type, Children, [], Options, GraphOptions),
    unicode:characters_to_binary(Output).

-spec make_dot_tree(iodata(), node_name(), node_text(), node_type(), node_children(), iodata(), node_options(), node_options()) -> iodata().
make_dot_tree(Prefix, Name, Text, Type, Children, Indent, Options, GraphOptions) ->
    Options2 = make_dot_options(Name, Type, Text, Options),
    Indent2 = [$\t|Indent],
    [
        Indent,     Prefix, " ", Name, " {\n",
        lists:map(fun(Option) -> [ Indent2, make_dot_option(Option), $;, $\n ] end, GraphOptions),
        Indent2,        Name, Options2, ";\n",
                        lists:map(fun(Child) -> make_dot_child(Name, Indent2, Child, Options) end, Children),
        Indent,     "}\n"
    ].

make_dot_options(Name, Type, Text, Options) when is_function(Options, 2) ->
    make_dot_options(Name, Type, Text, Options(Name, Type));
make_dot_options(_Name, _Type, [], []) ->
    do_make_dot_options([]);
make_dot_options(_Name, _Type, Text, []) ->
    do_make_dot_options([{label, Text}]);
make_dot_options(_Name, _Type, Text, Options) ->
    Options2 = case lists:keymember(label, 1, Options) of
        true -> Options;
        false when Text /= [] -> [{label, Text}|Options];
        false -> Options
    end,
    do_make_dot_options(Options2).

-spec do_make_dot_options(node_options()) -> iodata().
do_make_dot_options(Options) ->
    case lists:map(fun make_dot_option/1, Options) of
        [] ->
            [];
        Else ->
            [" [", add_separator(Else,$;), "]"]
    end.

make_dot_option(filled) -> "style = filled";
make_dot_option({style, Style}) -> ["style = ",Style];
make_dot_option({fillcolor, Color}) -> ["fillcolor = ",convert_color(Color)];
make_dot_option({shape, Shape}) -> ["shape = ",atom_to_list(Shape)];
make_dot_option({label, Text}) -> ["label = ",$",Text,$"];
make_dot_option({splines, Splines}) -> ["splines = ", atom_to_list(Splines)].

add_separator([Thing], _Sep) -> [Thing];
add_separator([Thing1, Thing2|Rest], Sep) -> [Thing1, Sep|add_separator([Thing2|Rest], Sep)];
add_separator([], _Sep) -> [].

make_dot_child(Parent, Indent, {node, Name, Text, worker, _Children}, Options) ->
    Options2 = make_dot_options(Name, worker, Text, Options),
    [
       Indent, Name, Options2, ";\n",
       Indent, Parent, " -> ", Name, ";\n"
    ];
make_dot_child(Parent, Indent, {node, Name, Text, Type, Children}, Options) ->
    [
        make_dot_tree("subgraph", Name, Text, Type, Children, Indent, Options, []),
        Indent, Parent, " -> ", Name, ";\n"
    ].

convert_color({R,G,B}) ->
    convert_color(io_lib:format("#~2.16.0B~2.16.0B~2.16.0B", [R,G,B]));
convert_color(Color) when is_atom(Color) ->
    convert_color(atom_to_list(Color));
convert_color(Color) when is_list(Color); is_binary(Color) ->
    [$",Color,$"].

graph(undefined) ->
    error(badarg);
graph(Sup) when is_atom(Sup) ->
    graph(whereis(Sup));
graph(Sup) when is_pid(Sup) ->
    {Name, Names} = get_name(Sup, dict:new()),
    element(1, graph(Name, Sup, Names)).


graph(Name, Sup) ->
    element(1, graph(Name, Sup, dict:new())).

%% TODO: Lots of boilerplate here. Simplify.
graph(Name, Sup, Names) ->
    PrettyName = pretty_name(Name),
    PrettyText = io_lib:format("~p", [Name]),
    case supervisor:which_children(Sup) of
        [] ->
            {ChildName, Names2} = assert_name("unknown", Names),
            PrettyChildName = pretty_name(ChildName),
            PrettyChildText = "???",
            {{node, PrettyName, PrettyText, simple_sup, [{node, PrettyChildName, PrettyChildText, worker, ChildName}]}, Names2};
        [{undefined, Pid, supervisor, _Modules}|_Rest] ->
            {ChildName, Names2} = get_simple_name(Sup, Pid, Names),
            {Res, Names3} = graph(ChildName, Pid, Names2),
            {{node, PrettyName, PrettyText, simple_sup, [Res]}, Names3};
        [{undefined, Pid, worker, _Modules}|_Rest] ->
            {ChildName, Names2} = get_simple_name(Sup, Pid, Names),
            PrettyChildName = pretty_name(ChildName),
            PrettyChildText = io_lib:format("~p", [ChildName]),
            {{node, PrettyName, PrettyText, simple_sup, [{node, PrettyChildName, PrettyChildText, worker, ChildName}]}, Names2};
        Children ->
            {GraphChildren, Names2} = lists:mapfoldl(fun
                    ({ChildName, _Pid, worker, _Modules}, NamesAcc) ->
                        {ChildName2, NamesAcc2} = assert_name(ChildName, NamesAcc),
                        PrettyChildName = pretty_name(ChildName2),
                        PrettyChildText = io_lib:format("~p", [ChildName2]),
                        {{node, PrettyChildName, PrettyChildText, worker, []}, NamesAcc2};
                    ({ChildName, Pid, supervisor, _Modules}, NamesAcc) ->
                        {ChildName2, NamesAcc2} = assert_name(ChildName, NamesAcc),
                        graph(ChildName2, Pid, NamesAcc2)
                end, Names, Children),
            %% TODO: Be smarter here. Max children should be configurable, and we should
            %% attempt to detect pseudo-simple supervisors (where someone uses a list
            %% comprehension to build N workers).
            case length(GraphChildren) of
                N when N < ?MAX_CHILDREN ->
                    {{node, PrettyName, PrettyText, sup, GraphChildren}, Names2};
                _N ->
                    {Taken, _} = lists:split(?MAX_CHILDREN, GraphChildren),
                    {{node, PrettyName, PrettyText, sup, Taken}, Names2}
            end
    end.

get_name(Pid, Names) ->
    Name = case process_info(Pid, registered_name) of
        [] ->
            {initial_call, {Mod, _, _}} = process_info(Pid, initial_call),
            Mod;
        {registered_name, Registered} ->
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
