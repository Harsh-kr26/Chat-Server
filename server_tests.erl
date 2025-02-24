-module(server_tests).
-include_lib("eunit/include/eunit.hrl").


setup() ->
    {ok, _} = server:start_link(10),
    ok.

cleanup(_) ->
    server:stop(),
    ok.


mock_client() ->
    spawn(fun() -> mock_client_loop([]) end).

mock_client_loop(Messages) ->
    receive
        {get_messages, Pid} ->
            Pid ! {messages, Messages},
            mock_client_loop(Messages);
        Msg ->
            mock_client_loop([Msg | Messages])
    end.


get_messages(Pid) ->
    Pid ! {get_messages, self()},
    receive
        {messages, Messages} -> lists:reverse(Messages)
    after 1000 -> 
        timeout
    end.



connection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [
            {"Connect user successfully",
             fun() ->
                Client = mock_client(),
                ?assertEqual({ok, "Welcome, user1"}, server:connect(Client, "user1"))
             end},
            
            {"Reject duplicate username",
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),
                {ok, _} = server:connect(Client1, "user2"),
                ?assertMatch({error, "Username already taken."}, server:connect(Client2, "user2"))
             end},
            
            {"First user becomes admin",
             fun() ->
                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),
                
                Client = mock_client(),
                {ok, _} = server:connect(Client, "admin_test_user"),
                
                % Request user list to verify admin status
                ok = server:request_user_list("admin_test_user"),
                timer:sleep(50), % Give time for message to arrive
                
                Messages = get_messages(Client),
                ?assertMatch([{user_list, ["admin_test_user"], ["admin_test_user"]} | _], Messages)
             end},
            
            {"Reject connection when server full",
             fun() ->
                % Start a server with max 1 user
                server:stop(),
                {ok, _} = server:start_link(1),
                
                Client1 = mock_client(),
                Client2 = mock_client(),
                {ok, _} = server:connect(Client1, "user_a"),
                ?assertMatch({error, "Server full, max1users allowed."}, server:connect(Client2, "user_b"))
             end}
        ]
     end}.


message_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [
            {"Send broadcast message",
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),
                {ok, _} = server:connect(Client1, "sender"),
                {ok, _} = server:connect(Client2, "receiver"),
                
                server:send_message("sender", "Hello everyone"),
                timer:sleep(50), % Give time for message to arrive
                
                Messages = get_messages(Client2),
                ?assertMatch([{msg, "sender" ++ _} | _], Messages)
             end},
             
            {"Send private message",
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),
                {ok, _} = server:connect(Client1, "alice"),
                {ok, _} = server:connect(Client2, "bob"),
                
                server:send_private_message("alice", "bob", "Secret message"),
                timer:sleep(50),
                
                BobMessages = get_messages(Client2),
                ?assertMatch([{msg, "[Private from alice] Secret message"} | _], BobMessages)
             end},
             
            {"Private message to non-existent user",
             fun() ->
                Client = mock_client(),
                {ok, _} = server:connect(Client, "messenger"),
                
                server:send_private_message("messenger", "ghost", "Hello?"),
                timer:sleep(50),
                
                Messages = get_messages(Client),
                ?assertMatch([{error, "User ghost not found."} | _], Messages)
             end},
             
            {"Message history is maintained",
             fun() ->
                Client = mock_client(),
                {ok, _} = server:connect(Client, "historian"),
                
                % Send multiple messages
                server:send_message("historian", "Message 1"),
                server:send_message("historian", "Message 2"),
                server:send_message("historian", "Message 3"),
                timer:sleep(50),
                
                server:get_message_history("historian"),
                timer:sleep(50),
                
                Messages = get_messages(Client),
                HistoryMsg = lists:keyfind(message_history, 1, Messages),
                ?assertNotEqual(false, HistoryMsg),
                {message_history, History} = HistoryMsg,
                ?assertEqual(3, length(History))
             end}
        ]
     end}.