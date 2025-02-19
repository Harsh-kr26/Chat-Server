-module(server).
-behaviour(gen_server).
-export([start_link/1, stop/0]).
-export([connect/2, disconnect/1, send_message/2, send_private_message/3,
         request_user_list/1, change_topic/2, make_admin/2, remove_admin/2, kick_user/2,
         get_message_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {users = [], max_users, current_topic = "General", admins = [], 
                message_history = [], history_size = 5}).

start_link(MaxUsers) when is_integer(MaxUsers), MaxUsers > 0 ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [MaxUsers], []).

stop() ->
    gen_server:stop(?MODULE).

connect(Pid, Username) ->
    gen_server:call(?MODULE, {connect, Pid, Username}).

disconnect(Username) ->
    gen_server:cast(?MODULE, {disconnect, Username}).

send_message(Username, Msg) ->
    gen_server:cast(?MODULE, {message, Username, Msg}).

send_private_message(Sender, Recipient, Msg) ->
    gen_server:call(?MODULE, {private_message, Sender, Recipient, Msg}).

request_user_list(Username) ->
    gen_server:call(?MODULE, {request_user_list, Username}).

change_topic(Username, NewTopic) ->
    gen_server:cast(?MODULE, {change_topic, Username, NewTopic}).

make_admin(AdminUsername, TargetUsername) ->
    gen_server:cast(?MODULE, {make_admin, AdminUsername, TargetUsername}).

remove_admin(AdminUsername, TargetUsername) ->
    gen_server:cast(?MODULE, {remove_admin, AdminUsername, TargetUsername}).

kick_user(AdminUsername, TargetUsername) ->
    gen_server:cast(?MODULE, {kick_user, AdminUsername, TargetUsername}).


get_message_history(Username) ->
    gen_server:call(?MODULE, {get_message_history, Username}).

%% gen_server callbacks
init([MaxUsers]) ->
    {ok, #state{max_users = MaxUsers}}.

%% Handle synchronous calls
handle_call({connect, Pid, Username}, _From, State) ->
    #state{users = Users, max_users = MaxUsers, admins = Admins, message_history = History} = State,
    case lists:keyfind(Username, 1, Users) of
        false when length(Users) < MaxUsers ->
            NewUsers = [{Username, Pid} | Users],
            NewAdmins = case length(Admins) of
                0 -> [Username];
                _ -> Admins
            end,
            broadcast(NewUsers, Username ++ " has joined the chat."),
            
            % Send message history to the new user
            case History of
                [] -> ok;
                _ -> Pid ! {message_history, History}
            end,
            
            {reply, {ok, "Welcome, " ++ Username}, 
             State#state{users = NewUsers, admins = NewAdmins}};
        false ->
            {reply, {error, "Server full, max" ++ integer_to_list(MaxUsers) ++ "users allowed."},
             State};
        _ ->
            broadcast(Users, "Username " ++ Username ++ " already taken."),
            {reply, {error, "Username already taken."}, State}
    end;

handle_call({private_message, Sender, Recipient, Msg}, _From, State) ->
    #state{users = Users} = State,
    case lists:keyfind(Recipient, 1, Users) of
        {Recipient, Pid} ->
            Pid ! {msg, "[Private from " ++ Sender ++ "] " ++ Msg},
            {reply, ok, State};
        false ->
            {Sender, SenderPid} = lists:keyfind(Sender, 1, Users),
            SenderPid ! {error, "User " ++ Recipient ++ " not found."},
            {reply, {error, user_not_found}, State}
    end;

handle_call({request_user_list, Username}, _From, State) ->
    #state{users = Users, admins = Admins} = State,
    case lists:keyfind(Username, 1, Users) of
        {Username, Pid} ->
            Usernames = [U || {U, _} <- Users],
            Pid ! {user_list, Usernames, Admins},
            {reply, ok, State};
        false ->
            {reply, {error, not_connected}, State}
    end;


handle_call({get_message_history, Username}, _From, State) ->
    #state{users = Users, message_history = History} = State,
    case lists:keyfind(Username, 1, Users) of
        {Username, Pid} ->
            Pid ! {message_history, History},
            {reply, ok, State};
        false ->
            {reply, {error, not_connected}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.


handle_cast({message, Username, Msg}, State) ->
    #state{users = Users, message_history = History, history_size = HistorySize} = State,
    Timestamp = calendar:local_time(),
    FormattedTimestamp = lists:flatten(io_lib:format("~p", [Timestamp])),
    FormattedMsg = "[" ++ Username ++ " - " ++ FormattedTimestamp ++ "] " ++ Msg,
    broadcast(Users, FormattedMsg),
    
    % Add message to history and maintain history size
    NewHistory = add_to_history(FormattedMsg, History, HistorySize),
    
    {noreply, State#state{message_history = NewHistory}};

handle_cast({change_topic, Username, NewTopic}, State) ->
    #state{users = Users, message_history = History, history_size = HistorySize} = State,
    TopicMsg = Username ++ " changed the topic to: " ++ NewTopic,
    broadcast(Users, TopicMsg),
    
    % Add topic change to history
    NewHistory = add_to_history(TopicMsg, History, HistorySize),
    
    {noreply, State#state{current_topic = NewTopic, message_history = NewHistory}};

handle_cast({make_admin, AdminUsername, TargetUsername}, State) ->
    #state{users = Users, admins = Admins, message_history = History, history_size = HistorySize} = State,
    case lists:member(AdminUsername, Admins) of
        true ->
            NewAdmins = lists:usort([TargetUsername | Admins]),
            AdminMsg = TargetUsername ++ " has been made an admin by " ++ AdminUsername,
            broadcast(Users, AdminMsg),
            
            % Add admin change to history
            NewHistory = add_to_history(AdminMsg, History, HistorySize),
            
            {noreply, State#state{admins = NewAdmins, message_history = NewHistory}};
        false ->
            broadcast(Users, AdminUsername ++ " is not an admin "),
            {noreply, State}
    end;

handle_cast({remove_admin, AdminUsername, TargetUsername}, State) ->
    #state{users = Users, admins = Admins, message_history = History, history_size = HistorySize} = State,
    case lists:member(AdminUsername, Admins) of
        true ->
            NewAdmins = lists:delete(TargetUsername, Admins),
            AdminMsg = TargetUsername ++ " is no longer an admin.",
            broadcast(Users, AdminMsg),
            
            % Add admin removal to history
            NewHistory = add_to_history(AdminMsg, History, HistorySize),
            
            {noreply, State#state{admins = NewAdmins, message_history = NewHistory}};
        false ->
            broadcast(Users, AdminUsername ++ " is not an admin "),
            {noreply, State}
    end;

handle_cast({kick_user, AdminUsername, TargetUsername}, State) ->
    #state{users = Users, admins = Admins, message_history = History, history_size = HistorySize} = State,
    case lists:member(AdminUsername, Admins) of
        true ->
            NewUsers = lists:keydelete(TargetUsername, 1, Users),
            KickMsg = TargetUsername ++ " has been kicked out by " ++ AdminUsername,
            broadcast(NewUsers, KickMsg),
            
            % Add kick event to history
            NewHistory = add_to_history(KickMsg, History, HistorySize),
            
            {noreply, State#state{
                users = NewUsers, 
                admins = lists:delete(TargetUsername, Admins),
                message_history = NewHistory
            }};
        false ->
            broadcast(Users, AdminUsername ++ " is not an admin "),
            {noreply, State}
    end;

handle_cast({disconnect, Username}, State) ->
    #state{users = Users, admins = Admins, message_history = History, history_size = HistorySize} = State,
    NewUsers = lists:keydelete(Username, 1, Users),
    DisconnectMsg = Username ++ " has left the chat.",
    broadcast(NewUsers, DisconnectMsg),
    
    % Add disconnect event to history
    NewHistory = add_to_history(DisconnectMsg, History, HistorySize),
    
    {noreply, State#state{
        users = NewUsers,
        admins = Admins,
        message_history = NewHistory
    }};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

broadcast(Users, Message) ->
    lists:foreach(fun({_, Pid}) -> Pid ! {msg, Message} end, Users).


add_to_history(Message, History, MaxSize) ->
    NewHistory = [Message | History],
    case length(NewHistory) > MaxSize of
        true -> lists:sublist(NewHistory, MaxSize);
        false -> NewHistory
    end.