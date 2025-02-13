-module(chat_client).
-export([start/2, listen/2, send_message/3,send_private_message/4, request_user_list/2, change_topic/3, make_admin/3, remove_admin/3, kick_user/3, disconnect/2]).

-type username() :: string().
-type server_pid() :: pid().
-type message() :: string().
-type topic() :: string().


-spec start(server_pid(), username()) -> pid().
start(ServerPid,Username) ->
    Pid = spawn(?MODULE, listen, [ServerPid,Username]),
    unlink(Pid),

    ServerPid ! {connect, Pid, Username},
    io:format("~p~n", [Pid]).
    
-spec listen(server_pid(), username()) -> no_return().
listen(Pid,Username) ->
    receive
        {msg, Message} ->
            io:format("[~p]  ~p~n", [self(),Message]),
            listen(Pid,Username);
        {user_list, Users, Admins} ->
            io:format("Connected Users: ~p~n", [Users]),
            io:format("Admins: ~p~n", [Admins]),
            listen(Pid, Username);
        {ok, WelcomeMsg} ->
            io:format("[~p]  ~p~n", [self(),WelcomeMsg]),
            {ok, Pid},
            listen(Pid,Username)
        %{error, Reason} ->"
         %   io:format("[~p]  Connection failed: ~p~n", [self(),Reason]),
          %  exit(normal)
    end.
-spec send_message(username(), server_pid(), message()) -> no_return().
send_message(Username,ServerPid, Msg) ->
    ServerPid ! {message, Username, Msg}.

send_private_message(Sender, ServerPid, Recipient, Msg) ->
    ServerPid ! {private_message, Sender, Recipient, Msg}.

request_user_list(Username, ServerPid) ->
    ServerPid ! {request_user_list, Username}.

-spec change_topic(username(), server_pid(), topic()) -> no_return().
change_topic(Username, ServerPid, NewTopic) ->
    ServerPid ! {change_topic, Username, NewTopic}.

-spec make_admin(username(), server_pid(), username()) -> no_return().
make_admin(AdminUsername, ServerPid, TargetUsername) ->
    ServerPid ! {make_admin, AdminUsername, TargetUsername}.

-spec remove_admin(username(), server_pid(), username()) -> no_return().
remove_admin(AdminUsername, ServerPid, TargetUsername) ->
    ServerPid ! {remove_admin, AdminUsername, TargetUsername}.

-spec kick_user(username(), server_pid(), username()) -> no_return().
kick_user(AdminUsername, ServerPid, TargetUsername) ->
    ServerPid ! {kick, AdminUsername, TargetUsername}.


-spec disconnect(username(), server_pid()) -> no_return().
disconnect(Username,ServerPid) ->
    ServerPid ! {disconnect, Username},
    exit(self(), normal).
