-module(music_analysis).
-import(mr).
-import(read_mxm).
-export([get_data/1,total_words/1,word_stats/1,grep/2,reverse/1]).


% Helper function - parses given file
get_data(InputFile) ->
    Parsed = read_mxm:from_file(InputFile),
    case Parsed of
        {Words, Tracks} ->
            {data,Words,Tracks};
        _ ->
            {error,parsing_problem}
    end.

    
%% Compute total number of words in all songs together
    % For each song, get the counts (discard the actual words)

% Map function:
% @spec total_words(Track :: binary()) -> integer())
track_words(Track) ->
    {_, _, Counts} = read_mxm:parse_track(Track),
    % io:format("Song: ~p, ~p words~n", [SongID,length(Counts)]),
    lists:foldl(fun({_, C}, AccIn) -> AccIn+C end, 0, Counts).

% Reduce function:
% @spec reduce_wordcount(Count :: integer(), Acc :: integer()) -> integer())
reduce_wordcount(Count,Acc) ->
    Count+Acc.
        
% Run this to perform the computation
total_words(InputFile) ->
    {data,_,Tracks} = get_data(InputFile),
    {ok,MR} = mr:start(8),
    {ok,Count} = mr:job(MR,
        fun track_words/1,
        fun reduce_wordcount/2,
        0,
        Tracks),
    mr:stop(MR),
    Count.

%% Compute average number of different words a song, and average total number of words in a song

    % For each song, get the number of different words and total words
    
    % Reduce by averaging the results
    
word_analysis(Track) ->
    {_, _, Counts} = read_mxm:parse_track(Track),
    Total = lists:foldl(fun({_, C}, AccIn) -> AccIn+C end, 0, Counts),
    Different = length(Counts),
    {word_data,Different,Total}.

    % I redid this so it sums the counts - with the averaging here you get a floating point operation every time!!
total_words({word_data,Different,Total},{totalled,DiffTotal,TotalTotal}) ->
    NewDiff = (Different+DiffTotal),
    NewTotal = (Total+TotalTotal),
    {totalled,NewDiff,NewTotal}.
        
word_stats(InputFile) ->
    {data,_,Tracks} = get_data(InputFile),
    Num = length(Tracks),
    {ok,MR} = mr:start(1000),
    {ok,Results} = mr:job(MR,
        fun word_analysis/1,
        fun total_words/2,
        {totalled,0,0},
        Tracks),    
    mr:stop(MR),
    {ok,{totalled,DiffTop,TotalTop}} = Results,
    % Perform the division here!
    Diff=DiffTop/Num,
    Total=TotalTop/Num,
    {{mean_different,Diff},{mean_total,Total}}.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grep

%% Finds the songs with the word supplied to function grep/2.
    % Assumption is that the word is spelled the same as in our dictionary.

    % index is needed to get the "number" key
    % of our word. Technically, we could do it
    % the long way by looking up every number
    % in each track in the dictionary, and then
    % comparing it to the chosen word, but I
    % felt this was easier

   
    %% grep_checkmember is used for the map function:
    % It checks if the given word's key is an element
    % of the track. If so, returns a singleton trackid
    % (useful in the reduce step) or false.
    
    grep_checkmember(Key,Track) ->
        {TrackId,_,Words} = read_mxm:parse_track(Track),
        Var = lists:keysearch(Key,1,Words),
        case Var of
            false -> false;
            _ -> [TrackId] % return as singleton list
        end.

    % grep_merge is the reduce function. It combines
    % two lists, or ignores an input if false.
    % Note: No ordering is intentionally kept.

    grep_merge(false,false) -> [];
    grep_merge(false,X) -> X;
    grep_merge(X,false) -> X;
    grep_merge([],X) -> X;
    grep_merge([L|Left],Right) -> grep_merge(Left,[L|Right]).
    
    
    %% Helper method for grep
    % index(Word,List): finds the position of Word in List
    index(Word,List) -> index_of(Word,List,1).    
    index_of(_,[],_) -> not_found;
    index_of(Word,[Word|_],Var) -> Var;
    index_of(Word,[_|Tail],Var) -> index_of(Word,Tail,Var+1).
    % Pulls every track id and mxm number containing Word

    % returns a list of track IDs for the given word, if it is in the dataset.
grep(Word,InputFile) ->
    {data,Words,Tracks} = get_data(InputFile),
    {ok,MR} = mr:start(8),
    LowerCase = string:to_lower(Word),
    Wordkey = index(LowerCase,Words),
    case Wordkey of
        not_found -> Ids = {fail, "Word not found"};
        _ -> {ok,Ids} = mr:job(MR,
            fun (X) -> grep_checkmember(Wordkey,X) end,
            fun grep_merge/2,
            [],
            Tracks)
    end,
    mr:stop(MR),
    Ids.
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Reverse indexing

% helper methods:


    % MAP function:
    % given a track, create dictionary of words with the track ID
    reverse_tuple_dict(Track) ->
        {TrackId,_,Words} = read_mxm:parse_track(Track),
        Dict = reverse_makedict(Words,TrackId,dict:new()),
        Dict.

    % Make a dictionary for the given list of words and song ID
    reverse_makedict([],_,Dict) -> Dict;
    reverse_makedict([{Head,Val}|Tail],X,Dict) ->
        Word = Head,
        NewDict = dict:append(Word,{X,Val},Dict),
        reverse_makedict(Tail,X,NewDict).
  
        
    % REDUCE function: 
    % Takes 2 dicts and merges the values (lists) for the same keys
    reverse_merge(D1,D2) ->
        dict:merge(fun(Key,V1,V2) -> lists:append(V1,V2) end,D1,D2).

    % convert word ids to natural language words
    reverse_num_to_word(Dict,Words) -> 
        Keys = dict:fetch_keys(Dict),
        reverse_num_to_word_work(Dict,Words,Keys,dict:new()).


    reverse_num_to_word_work(_,_,[],NewDict) -> NewDict;
    reverse_num_to_word_work(Dict,Words,[Key|Keys],NewDict) ->
        Word = reverse_safety(Key,Words),
        Value = dict:fetch(Key,Dict),
        Update = dict:append(Word,Value,NewDict),
        reverse_num_to_word_work(Dict,Words,Keys,Update).
    
        reverse_safety(Index,List) ->
            case Index > length(List) of
                false -> lists:nth(Index,List);
                true -> Index
            end.

            % Finally, the
reverse(InputFile) ->
    {data,Words,Tracks} = get_data(InputFile),
    {ok,MR} = mr:start(128),
    {ok,Reverse} = mr:job(MR,
        fun reverse_tuple_dict/1,
        fun reverse_merge/2,
        dict:new(),
        Tracks),
    {ok, Temp} = Reverse,
    reverse_num_to_word(Temp,Words).
    