# superdot
DOT-file generator for Erlang supervision trees

## Basic Usage
**Note**: This example only works if you start erl with the sname or name flag at present (because net_sup is not started without it). This will be corrected in the future.
```erlang
Erlang/OTP 17 [erts-6.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V6.3  (abort with ^G)
(juno@diogenes)1> Graph = superdot:graph(kernel_sup).
{graph,{registered_name,kernel_sup},
       normal,
       [{graph,kernel_safe_sup,simple,[{node,"unknown"}]},
        {node,kernel_config},
        {graph,user,simple,[{node,{"unknown",2}}]},
        {graph,standard_error,simple,[{node,{"unknown",3}}]},
        {node,code_server},
        {node,file_server_2},
        {node,global_group},
        {graph,net_sup,normal,
               [{node,net_kernel},{node,auth},{node,erl_epmd}]},
        {node,inet_db},
        {node,global_name_server},
        {node,rex}]}
(juno@diogenes)2> String = superdot:to_dot(Graph).
<<"digraph registered_name_kernel_sup{\nregistered_name_kernel_sup [shape=box,style=filled,fillcolor=\"#ffcccc\"];\nsubgrap"...>>
(juno@diogenes)3> file:write_file("/tmp/test.dot", String).
ok
```

My intent is to allow you to eventually write directly to an image file (or even a dot file for that matter).
