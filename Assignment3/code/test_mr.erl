-module(test_mr).
-import(mr).
-import(read_mxm).
-export([test_readmxm/1,test_sum/0]).

test_readmxm(File) ->
    {Words,_} = read_mxm:from_file(File),
    length(Words).
    
% Simple example for summing and finding the factorial of numbers 1 to 10
test_sum() ->
    {ok,MR} = mr:start(3),
    {ok,Sum} = mr:job(MR,
                        fun(X) -> X end,
                        fun(X,Acc) -> X+Acc end,
                        0,
                        lists:seq(1,10)),
    {ok, Fac} = mr:job(MR,
                               fun(X) -> X end,
                               fun(X,Acc) -> X*Acc end,
                               1,
                               lists:seq(1,10)),
    mr:stop(MR),
    {Sum,Fac}.
