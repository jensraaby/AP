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

% Job function
% Set the functions
% Send the data
job(CPid, MapFun, RedFun, RedInit, Data) ->
    ok = rpc(CPid,{mapper_setup,MapFun}), %% ensure the mappers ready using a synchronous call
    L = length(Data), % for bookkeeping
    io:format("Input: ~p  items~n", [L]),
    data_async(CPid,Data),
    % getting the result should be synchronous
    Result = rpc(CPid,{reducer_setup,RedFun,RedInit,length(Data)}),
    {ok,Result}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Internal implementation

init(N) -> 
    Reducer = spawn(fun() -> reducer_loop() end),
    io:format("Started reducer ~p ~n", [Reducer]),
    Mappers = for(1,N,fun() -> spawn(fun() -> mapper_loop(Reducer,unready,function_undefined) end) end),
    io:format("Started ~p mappers ~n", [length(Mappers)]),
    {Reducer,Mappers}.


% "For" loop construction (from Programming Erlang, Joe Armstrong)
for(N,N,F) -> [F()];
for(I,N,F) -> [F()|for(I+1,N,F)].


%%% Coordinator

coordinator_loop(Reducer, Mappers) ->
    % Link the child processes: this has no effect after first run
    link(Reducer),
    lists:foreach(fun link/1, Mappers),
        
    % Receive messages
    receive
        {From, stop} ->
            io:format("~p stopping~n", [self()]),
            lists:foreach(fun stop_async/1, Mappers),
            stop_async(Reducer),
            reply_ok(From);
            
        {From,{mapper_setup,MapFun}} ->
            io:format("~p setting up mappers~n", [self()]),
            lists:foreach(fun(M) -> setup_async(M,MapFun) end,Mappers),
            reply_ok(From), % here we could have a protocol for the mappers saying they are ready
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

%%% Send data just assumes that all the data is received by a working mapper
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
        ready -> %not used at present
            ok,
            reducer_loop();
        {From,{start_and_gather,Fun,I,N}} ->
            io:format("Reducer ~p starting to wait~n", [self()]),
            Result = gather_data_from_mappers(Fun,I,N),
            reply(From,Result),
            reducer_loop()
        % this seems to cause problems:
        % Unknown ->
        %          io:format("reducer: unknown message: ~p~n",[Unknown]), 
        %          reducer_loop()
    end.

gather_data_from_mappers(_,Acc,0) ->
    io:format("Reducer ~p received all data~n", [self()]),
    Acc;
    
gather_data_from_mappers(Fun, Acc, Missing) ->
    
    
    receive
        {data,D} ->
            if 
                Missing rem 100 == 0 ->
                    %io:format("reducer ~p currently at ~p~n", [self(),Acc]);
                    true;
                true ->
                    % io:format("Reducer ~p received data: ~p. ~p remaining~n", [self(),D,Missing]),
                    true
            end,
           gather_data_from_mappers(Fun,Fun(D,Acc),Missing-1)
    end.


%%% Mapper
mapper_loop(Reducer,Fun,State) ->
    receive
        stop -> 
            io:format("Mapper ~p stopping~n", [self()]),
            ok;
            
        {setup, NewFun} -> % Sets the loop to have the given Function
            % io:format("Mapper ~p starting ~n", [self()]),
            mapper_loop(Reducer,NewFun,ready);
            
        {data, D} -> % process and send result to reducer
            data_async(Reducer,Fun(D)),        
            mapper_loop(Reducer,Fun,State);
            
        {From,state} ->
            info(From,State),
            mapper_loop(Reducer,Fun,State);
            
        Unknown ->
            io:format("Mapper - unknown message: ~p~n",[Unknown]), 
            mapper_loop(Reducer, Fun,State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
