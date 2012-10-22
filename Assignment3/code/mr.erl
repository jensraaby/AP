%%%% MapReduce Framework in Erlang
%%%% October 2012
%%%% E.J.Partridge & J.P.Raaby
%% Advanced Programming assignment 3

-module(mr).
-export([start/1, stop/1, job/5]).

%%%% Interface

% Starts a Mapreduce coordinator with N mapper nodes
start(N) ->
    {Reducer, Mappers} = init(N),
    {ok, spawn(fun() -> coordinator_loop(Reducer, Mappers) end)}.


stop(Pid) ->
    % send a synchronous message to the coordinator
    rpc(Pid, stop).
    % Pid ! die.
    %TODO check this

% Job function
% Set the functions
% Send the data
job(CPid, MapFun, RedFun, RedInit, Data) ->
    info(CPid,{mapper_setup,MapFun}),
    % io:format("Input: ~p ~n", [Data]),
    info(CPid,{data,Data}),
    % getting the result should be synchronous
    Result = rpc(CPid,{reducer_setup,RedFun,RedInit,length(Data)}),
    {ok,Result}.


%%%% Internal implementation
init(N) -> 
    Reducer = spawn(fun() -> reducer_loop() end),
    io:format("Started reducer ~p ~n", [Reducer]),
    Mappers = for(1,N,fun() -> spawn(fun() -> mapper_loop(Reducer,unready) end) end),
    io:format("Started ~p mappers ~n", [length(Mappers)]),
    {Reducer,Mappers}.


% "For" loop construction (from Programming Erlang, Joe Armstrong)
for(N,N,F) -> [F()];
for(I,N,F) -> [F()|for(I+1,N,F)].

%% synchronous communication
% This is Joe Armstrong's syntax!
rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

reply_ok(From) ->
    reply(From, ok).

reply_ok(From, Msg) ->
    reply(From, {ok, Msg}).


%% asynchronous communication
info(Pid, Msg) ->
    Pid ! Msg.

stop_async(Pid) ->
    info(Pid, stop).

data_async(Pid, D) ->
    info(Pid, {data, D}).

setup_async(Pid, Fun) ->
    info(Pid, {setup, Fun}).


%%% Coordinator

coordinator_loop(Reducer, Mappers) ->
    receive
        {From, stop} ->
            io:format("~p stopping~n", [self()]),
            lists:foreach(fun stop_async/1, Mappers),
            stop_async(Reducer),
            reply_ok(From);
            
        {mapper_setup,MapFun} ->
            io:format("~p setting up mappers~n", [self()]),
            lists:foreach(fun(M) -> setup_async(M,MapFun) end,Mappers),
            coordinator_loop(Reducer,Mappers);
            
        {data,D} ->
            io:format("~p starting ~n", [self()]),
            send_data(Mappers,D),
            coordinator_loop(Reducer,Mappers);
            
        {From,{reducer_setup,RedFun,RedInit,Size}} ->
            io:format("~p setting up reducer~n", [self()]),
            reply_ok(From,rpc(Reducer,{start_and_gather,RedFun,RedInit,Size})),
            coordinator_loop(Reducer,Mappers);

        Unknown ->
            io:format("unknown message: ~p~n",[Unknown]), 
            coordinator_loop(Reducer,Mappers)
    end.


send_data(Mappers, Data) ->
    send_loop(Mappers, Mappers, Data).

send_loop(Mappers, [Mid|Queue], [D|Data]) ->
    data_async(Mid, D),
    send_loop(Mappers, Queue, Data);
send_loop(_, _, []) -> ok;
send_loop(Mappers, [], Data) ->
    send_loop(Mappers, Mappers, Data).


%%% Reducer
reducer_loop() ->
    receive
        stop -> 
            io:format("Reducer ~p stopping~n", [self()]),
            ok;
        {From,{start_and_gather,Fun,I,N}} ->
            io:format("Reducer ~p starting to wait~n", [self()]),
            Result = gather_data_from_mappers(Fun,I,N),
            reply(From,Result),
            reducer_loop()
            
        % Unknown ->
        %          io:format("reducer: unknown message: ~p~n",[Unknown]), 
        %          reducer_loop()
    end.

gather_data_from_mappers(_,Acc,0) ->
    io:format("Reducer ~p received all data: ~p~n", [self(),Acc]),
    Acc;
    
gather_data_from_mappers(Fun, Acc, Missing) ->
    % io:format("Reducer ~p waiting~n", [self()]),
    receive
        {data,D} ->
            % io:format("Reducer ~p received data: ~p. ~p remaining~n", [self(),D,Missing]),
            gather_data_from_mappers(Fun,Fun(D,Acc),Missing-1)
    end.


%%% Mapper
mapper_loop(Reducer,Fun) ->
    receive
        stop -> 
            io:format("Mapper ~p stopping~n", [self()]),
            ok;
            
        {setup, NewFun} -> % Sets the loop to have the given Function
            % io:format("Mapper ~p starting ~n", [self()]),
            mapper_loop(Reducer,NewFun);
            
        {data, D} -> % process and send to reducer
            data_async(Reducer,Fun(D)),
            % io:format("Mapper - got data: ~p~n",[D]), 
            mapper_loop(Reducer,Fun);
            
        Unknown ->
            io:format("Mapper - unknown message: ~p~n",[Unknown]), 
            mapper_loop(Reducer, Fun)
    end.
