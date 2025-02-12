-module(chat_client).
-export([start/2, listen/2, send_message/3, disconnect/2]).

start(ServerPid,Username) ->
    Pid = spawn(?MODULE, listen, [ServerPid,Username]),
    unlink(Pid),

    ServerPid ! {connect, Pid, Username},
    io:format("~p~n", [Pid]).
    

listen(Pid,Username) ->
    receive
        {msg, Message} ->
            io:format("[~p]  ~p~n", [self(),Message]),
            listen(Pid,Username);
        %{error, Msg} ->
         %   io:format("[~p]  Error: ~p~n", [self(),Msg]),
          %  exit(normal);
        {ok, WelcomeMsg} ->
            io:format("[~p]  ~p~n", [self(),WelcomeMsg]),
            {ok, Pid},
            listen(Pid,Username)
        %{error, Reason} ->"
         %   io:format("[~p]  Connection failed: ~p~n", [self(),Reason]),
          %  exit(normal)
    end.

send_message(Username,ServerPid, Msg) ->
    ServerPid ! {message, Username, Msg}.


disconnect(Username,ServerPid) ->
    ServerPid ! {disconnect, Username},
    exit(self(), normal).
