-module(music_analysis).
-import(mr).
-import(read_mxm).
-export([test_data/0,total_words/0,words/0]).



test_data() ->
    {Words, Tracks} = read_mxm:from_file(mxm_dataset_test.txt),
    {data,Words,Tracks}.


    
%% Compute total number of words in all songs together
    % For each song, get the counts (discard the actual words)
    
    % get the song ID,mxmID and the list of tuples: [{wid:cnt}]
    %read_mxm:parse_track()

total_words(Track) ->
    {_, _, Counts} = read_mxm:parse_track(Track),
    % io:format("Song: ~p, ~p words~n", [SongID,length(Counts)]),
    lists:foldl(fun({_, C}, AccIn) -> AccIn+C end, 0, Counts).

reduce_wordcount(Count,Acc) ->
    Count+Acc.
        
total_words() ->
    {data,_,Tracks} = test_data(),
    {ok,MR} = mr:start(8),
    {ok,Count} = mr:job(MR,
        fun total_words/1,
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

average_words({word_data,Different,Total},{averaged,DiffAvg,TotalAvg}) ->
    if 
        {DiffAvg,TotalAvg} =:= {0,0} ->
            NewDiff  = Different,
            NewTotal = Total;
        true ->
            NewDiff = (Different+DiffAvg)/2,
            NewTotal = (Total+TotalAvg)/2
    end,
    {averaged,NewDiff,NewTotal}.
        
words() ->
    {data,_,Tracks} = test_data(),
    {ok,MR} = mr:start(8),
    {ok,Results} = mr:job(MR,
        fun word_analysis/1,
        fun average_words/2,
        {averaged,0,0},
        Tracks),
    mr:stop(MR),
    {ok,{averaged,Diff,Total}} = Results,
    {Diff,Total}.