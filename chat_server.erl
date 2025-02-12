-module(chat_server).
-export([start/1, loop/2]).

start(MaxUsers) when is_integer(MaxUsers), MaxUsers > 0 ->
    Pid = spawn(?MODULE, loop, [[],MaxUsers]),
    unlink(Pid),
    Pid.


loop(Users,MaxUsers) ->
    receive
        {connect, Pid, Username} ->
            case lists:keyfind(Username, 1, Users) of
                false when length(Users) < MaxUsers ->
                    Pid ! {ok, "Welcome, " ++ Username},
                    NewUsers = [{Username, Pid} | Users],
                    broadcast(NewUsers, Username ++ " has joined the chat."),
                    loop(NewUsers,MaxUsers);
                false ->
                    Pid ! {error, "Server full, max" ++ integer_to_list(MaxUsers) ++ "users allowed."},
                    loop(Users,MaxUsers);
                _ ->
                    Pid ! {error, "Username already taken."},
                    loop(Users,MaxUsers)
            end;
        
        {message, Username, Msg} ->
            Timestamp = calendar:local_time(),
            FormattedTimestamp = io_lib:format("~p", [Timestamp]),
            Flatenned = lists:flatten(FormattedTimestamp),
            broadcast(Users, "[" ++ Username ++ " - " ++ Flatenned ++ "] " ++ Msg),
            loop(Users,MaxUsers);

    

        {disconnect, Username} ->
            NewUsers = lists:keydelete(Username, 1, Users),
            broadcast(NewUsers, Username ++ " has left the chat."),
            loop(NewUsers,MaxUsers)
    end.

broadcast(Users, Message) ->
    lists:foreach(fun({_, Pid}) -> Pid ! {msg, Message} end, Users).
