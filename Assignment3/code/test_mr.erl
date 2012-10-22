-module(test_mr).
-import(mr).
-import(read_mxm).
-export([test_jens/0,test_sum/0]).

test_readmxm(File) ->
    {Words,_} = read_mxm:from_file(File),
    length(Words).


test_jens() ->
    {ok,MR} = mr:start(5),
    mr:stop(MR).
    
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
% test() ->
%     % Read in the test file
%     
%     wc_dir(".").
%     
% wc_dir(Dir) ->
%     "hello".
%         %%%% setup the mapreduce here
%         
% generate_words(Pid,File) ->
%     F = fun(Word) -> Pid ! {Word, 1} end.
%     lib_misc:foreachWordInFile(File,F).
%             
% count_words(Key,Vals,A) ->
%     [{length(Vals), Key}|A].