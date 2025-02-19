-module(client).
-behaviour(gen_server).
-export([start/2, stop/1]).
-export([send_message/2, send_private_message/3, request_user_list/1,
         change_topic/2, make_admin/2, remove_admin/2, kick_user/2, disconnect/1, 
         request_message_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { server_ref, username }).

start(Server, Username) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Server, Username], []).

stop(ClientPid) ->
    gen_server:stop(ClientPid).

send_message(ClientPid, Msg) ->
    gen_server:cast(ClientPid, {send_message, Msg}).

send_private_message(ClientPid, Recipient, Msg) ->
    gen_server:cast(ClientPid, {send_private_message, Recipient, Msg}).

request_user_list(ClientPid) ->
    gen_server:cast(ClientPid, request_user_list).

change_topic(ClientPid, NewTopic) ->
    gen_server:cast(ClientPid, {change_topic, NewTopic}).

make_admin(ClientPid, TargetUsername) ->
    gen_server:cast(ClientPid, {make_admin, TargetUsername}).

remove_admin(ClientPid, TargetUsername) ->
    gen_server:cast(ClientPid, {remove_admin, TargetUsername}).

kick_user(ClientPid, TargetUsername) ->
    gen_server:cast(ClientPid, {kick_user, TargetUsername}).

disconnect(ClientPid) ->
    gen_server:cast(ClientPid, disconnect).


request_message_history(ClientPid) ->
    gen_server:cast(ClientPid, request_message_history).

%% gen_server callbacks
init([ServerRef, Username]) ->
    case server:connect(self(), Username) of
        {ok, WelcomeMsg} ->
            io:format("[~p] ~p~n", [self(), WelcomeMsg]),
            {ok, #state{server_ref = ServerRef, username = Username}};
        {error, Reason} ->
            io:format("[~p] Connection failed: ~p~n", [self(), Reason]),
            {stop, {connection_failed, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast({send_message, Msg}, #state{username = Username} = State) ->
    server:send_message(Username, Msg),
    {noreply, State};

handle_cast({send_private_message, Recipient, Msg}, #state{username = Username} = State) ->
    server:send_private_message(Username, Recipient, Msg),
    {noreply, State};

handle_cast(request_user_list, #state{username = Username} = State) ->
    server:request_user_list(Username),
    {noreply, State};

handle_cast({change_topic, NewTopic}, #state{username = Username} = State) ->
    server:change_topic(Username, NewTopic),
    {noreply, State};

handle_cast({make_admin, TargetUsername}, #state{username = Username} = State) ->
    server:make_admin(Username, TargetUsername),
    {noreply, State};

handle_cast({remove_admin, TargetUsername}, #state{username = Username} = State) ->
    server:remove_admin(Username, TargetUsername),
    {noreply, State};

handle_cast({kick_user, TargetUsername}, #state{username = Username} = State) ->
    server:kick_user(Username, TargetUsername),
    {noreply, State};

handle_cast(request_message_history, #state{username = Username} = State) ->
    server:get_message_history(Username),
    {noreply, State};

handle_cast(disconnect, #state{username = Username} = State) ->
    server:disconnect(Username),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({msg, Message}, State) ->
    io:format("[~p] ~p~n", [self(), Message]),
    {noreply, State};

handle_info({user_list, Users, Admins}, State) ->
    io:format("Connected Users: ~p~n", [Users]),
    io:format("Admins: ~p~n", [Admins]),
    {noreply, State};


handle_info({message_history, Messages}, State) ->
    io:format("~n--- Chat History (most recent first) ---~n"),
    lists:foreach(fun(Msg) ->
        io:format("~p~n", [Msg])
    end, Messages),
    io:format("--- End of History ---~n~n"),
    {noreply, State};

handle_info({error, Reason}, State) ->
    io:format("[~p] Error: ~p~n", [self(), Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{username = Username}) ->
    catch server:disconnect(Username),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.