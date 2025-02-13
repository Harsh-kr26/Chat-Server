-module(chat_server).
-export([start/1, loop/4]).

-type username() :: string().
-type server_pid() :: pid().
-type user_list() :: [{username(), server_pid()}].
-type max_users() :: pos_integer().
-type topic() :: string().
-type admin_list() :: [username()].
-type message() :: string().


-spec start(max_users()) -> server_pid().
start(MaxUsers) when is_integer(MaxUsers), MaxUsers > 0 ->
    Pid = spawn(?MODULE, loop, [[],MaxUsers,"General",[]]),
    unlink(Pid),
    Pid.

-spec loop(user_list(), max_users(), topic(), admin_list()) -> no_return().
loop(Users,MaxUsers,CurrentTopic,Admins) ->
    receive
        {connect, Pid, Username} ->
            case lists:keyfind(Username, 1, Users) of
                false when length(Users) < MaxUsers ->
                    Pid ! {ok, "Welcome, " ++ Username},
                    NewUsers = [{Username, Pid} | Users],
                    NewAdmins = case length(Admins) of
                        0 -> [Username];  
                        _ -> Admins
                    end,
                    broadcast(NewUsers, Username ++ " has joined the chat."),
                    loop(NewUsers,MaxUsers,CurrentTopic,NewAdmins);
                false ->
                    Pid ! {error, "Server full, max" ++ integer_to_list(MaxUsers) ++ "users allowed."},
                    loop(Users,MaxUsers,CurrentTopic,Admins);
                _ ->
                    Pid ! {error, "Username already taken."},
                    broadcast(Users,"Username " ++  Username ++ " already taken."),
                    loop(Users,MaxUsers,CurrentTopic,Admins)
            end;
        
        {message, Username, Msg} ->
            Timestamp = calendar:local_time(),
            FormattedTimestamp = io_lib:format("~p", [Timestamp]),
            Flatenned = lists:flatten(FormattedTimestamp),
            broadcast(Users, "[" ++ Username ++ " - " ++ Flatenned ++ "] " ++ Msg),
            loop(Users,MaxUsers,CurrentTopic,Admins);

        {private_message, Sender, Recipient, Msg} ->
            case lists:keyfind(Recipient, 1, Users) of
                {Recipient, Pid} ->
                    Pid ! {msg, "[Private from " ++ Sender ++ "] " ++ Msg},
                    loop(Users, MaxUsers, CurrentTopic, Admins);
                false ->
                    lists:keyfind(Sender, 1, Users) ! {error, "User " ++ Recipient ++ " not found."},
                    loop(Users, MaxUsers, CurrentTopic, Admins)
            end;

        {request_user_list, Username} ->
            case lists:keyfind(Username, 1, Users) of
                {Username, Pid} ->
                    Usernames = [U || {U, _} <- Users],
                    Pid ! {user_list, Usernames, Admins},
                    loop(Users, MaxUsers, CurrentTopic, Admins);
                false ->
                    loop(Users, MaxUsers, CurrentTopic, Admins)
            end;

        {change_topic, Username, NewTopic} ->
            broadcast(Users, Username ++ " changed the topic to: " ++ NewTopic),
            loop(Users, MaxUsers, NewTopic,Admins);

        {make_admin, AdminUsername, TargetUsername} ->
            case lists:member(AdminUsername, Admins) of
                true ->
                    NewAdmins = lists:usort([TargetUsername | Admins]),
                    broadcast(Users, TargetUsername ++ " has been made an admin by " ++ AdminUsername),
                    loop(Users, MaxUsers, CurrentTopic, NewAdmins);
                false ->
                    broadcast(Users, AdminUsername ++ " is not an admin "),
                    loop(Users, MaxUsers, CurrentTopic, Admins)
            end;

        {remove_admin, AdminUsername, TargetUsername} ->
            case lists:member(AdminUsername, Admins) of
                true ->
                    NewAdmins = lists:delete(TargetUsername, Admins),
                    broadcast(Users, TargetUsername ++ " is no longer an admin."),
                    loop(Users, MaxUsers, CurrentTopic, NewAdmins);
                false ->
                    broadcast(Users, AdminUsername ++ " is not an admin "),
                    loop(Users, MaxUsers, CurrentTopic, Admins)
            end;

        {kick, AdminUsername, TargetUsername} ->
            case lists:member(AdminUsername, Admins) of
                true ->
                    NewUsers = lists:keydelete(TargetUsername, 1, Users),
                    broadcast(NewUsers, TargetUsername ++ " has been kicked out by " ++ AdminUsername),
                    loop(NewUsers, MaxUsers, CurrentTopic, lists:delete(TargetUsername, Admins));
                false ->
                    broadcast(Users, AdminUsername ++ " is not an admin "),
                    loop(Users, MaxUsers, CurrentTopic, Admins)
            end;

        {disconnect, Username} ->
            NewUsers = lists:keydelete(Username, 1, Users),
            broadcast(NewUsers, Username ++ " has left the chat."),
            loop(NewUsers,MaxUsers,CurrentTopic,Admins)
    end.

-spec broadcast(user_list(), message()) -> ok.
broadcast(Users, Message) ->
    lists:foreach(fun({_, Pid}) -> Pid ! {msg, Message} end, Users).
