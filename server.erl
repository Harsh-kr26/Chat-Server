-module(server).
-behaviour(gen_server).

-define(Max_Clients, 10).
-define(Max_History, 10).

-export([start_server/0, send_message/2, list_clients/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(client, {username, pid}).
-record(message, {sender, timestamp, text}).
-record (state, {clients = [], message_history = []}).

start_server() ->
    gen_server:start_server({local, ?MODULE}, ?MODULE, [], []).

handle_call({send_message, Sender, Msg}, _From, State) ->
    NewState = handle_new_message(Sender, Msg, State),
    broadcast_message = (Sender, Msg, NewState),
    {noreply, NewState};

handle_call({private_message, Sender, Reciever}, _From, State) ->
    PrivateMsg = io:format("Private message from ~s to ~s-n", [Sender, Reciever]),
    {reply, PrivateMsg, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_new_message(Sender, Msg, State) ->
    Timestamp = erlang:now(),
    Message = #message{sender = Sender, timestamp = Timestamp, text = Msg},
    NewHistory = [Message | State#state.message_history],
    NewHistoryTrimmed = lists:sublist(NewHistory, 1, ?Max_History),
    State#state[message_history = NewHistoryTrimmed].

broadcast_message(Sender, Msg, State) ->
    lists:foreach(fun(#client{pid = Pid}) ->
                gen_server:cast(Pid, {broadcast, Sender, Msg})
                ens, State#state, clients).

%% to be written functions:
%% handle_new_client

