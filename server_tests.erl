-module(server_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, _} = server:start_link(10),
    ok.

cleanup(_) ->
    server:stop(),
    ok.

%% Mock client process
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

%% Helper to get messages from mock client
get_messages(Pid) ->
    Pid ! {get_messages, self()},
    receive
        {messages, Messages} -> lists:reverse(Messages)
    after 1000 -> 
        timeout
    end.

%% Helper function to check message pattern after ignoring Timestamp
match_message_ignore_timestamp(Message, Username, Content) ->
    UserPrefix = "[" ++ Username ++ " - ",
    ContentSuffix = "] " ++ Content,
    
    % Check if message starts with the username prefix
    StartsWith = string:substr(Message, 1, length(UserPrefix)) =:= UserPrefix,
    
    % Find if the content suffix exists in the message
    HasContent = string:find(Message, ContentSuffix) =/= nomatch,
    
    % Both conditions must be true
    StartsWith andalso HasContent.

received_message_pattern(Messages, Username, Content) ->
    lists:any(
        fun(Msg) -> 
            case Msg of
                {msg, MsgContent} -> 
                    match_message_ignore_timestamp(MsgContent, Username, Content);
                _ -> false
            end
        end, 
        Messages).

%% ===== Test Cases =====

connection_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [
            {"Connect user successfully",                       %% === Correct =====
             fun() ->
                Client = mock_client(),
                ?assertEqual({ok, "Welcome, user1"}, server:connect(Client, "user1"))
             end},
            
            {"Reject duplicate username",                       %% === Correct =====
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),
                {ok, _} = server:connect(Client1, "user2"),
                ?assertMatch({error, "Username already taken."}, server:connect(Client2, "user2"))
             end},
            
            {"First user becomes admin",                        %% === Correct =====
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
                {user_list, Users, Admins} = lists:keyfind(user_list, 1, Messages),
                ?assert(lists:member("admin_test_user", Admins))
             end},
            
            {"Reject connection when server full",              %% === Correct =====
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
            {"Send broadcast message",                                  %% === Correct =====
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),

                {ok, _} = server:connect(Client1, "user1"),
                {ok, _} = server:connect(Client2, "user2"),

                % Send some messages
                server:send_message("user1", "First message"),

                % Request message history
                server:get_message_history("user1"),

                % Verify that the history is maintained and truncated if necessary
                Messages1 = get_messages(Client1),
                Messages2 = get_messages(Client2),

                ?assert(received_message_pattern(Messages1,"user1","First message")),
                ?assert(received_message_pattern(Messages1,"user1","First message"))
             end},
             
            {"Send private message",                                    %% === Correct =====
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),

                {ok, _} = server:connect(Client1, "Alice"),
                {ok, _} = server:connect(Client2, "Bob"),
                
                server:send_private_message("Alice", "Bob", "Secret message"),
                timer:sleep(50),
                
                BobMessages = get_messages(Client2),
                ReversedBobMessages = lists:reverse(BobMessages),
                ?assert(lists:member({msg,"[Private from Alice] Secret message"},ReversedBobMessages))
             end},
             
            {"Private message to non-existent user",                    %% === Correct =====
             fun() ->
                Client = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),

                {ok, _} = server:connect(Client, "messenger"),
                
                server:send_private_message("messenger", "ghost", "Hello?"),
                timer:sleep(50),
                
                Messages = get_messages(Client),
                [ _| LastMessage] = Messages,
                ?assert(lists:member({error,"User ghost not found."}, LastMessage))
             end},
             
            {"Message history is maintained",                           %% === Correct =====
             fun() ->
                Client1 = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),

                {ok, _} = server:connect(Client1, "user1"),

                % Send some messages
                server:send_message("user1", "First message"),
                server:send_message("user1", "Second message"),
                server:send_message("user1", "Third message"),
                server:send_message("user1", "Fourth message"),
                server:send_message("user1", "Fifth message"),
                server:send_message("user1", "Sixth message"),

                % Request message history
                server:get_message_history("user1"),

                % Verify that the history is maintained and truncated if necessary
                Messages1 = get_messages(Client1),
               
                % The history size should be 5, so the first message should be missing
                ?assert(received_message_pattern(Messages1, "user1", "Sixth message")),
                ?assert(received_message_pattern(Messages1, "user1", "Fifth message")),
                ?assert(received_message_pattern(Messages1, "user1", "Fourth message")),
                ?assert(received_message_pattern(Messages1, "user1", "Third message")),
                ?assert(received_message_pattern(Messages1, "user1", "Second message")),
                ?assert(received_message_pattern(Messages1, "user1", "First message"))
             end}
        ]
     end}.

admin_operations_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [
            {"Make user admin",                                                    %% === Correct =====
             fun() ->
                AdminClient = mock_client(),
                UserClient = mock_client(),
                {ok, _} = server:connect(AdminClient, "super_admin"),
                {ok, _} = server:connect(UserClient, "regular_user"),
                
                server:make_admin("super_admin", "regular_user"),
                timer:sleep(50),
                
                server:request_user_list("super_admin"),
                timer:sleep(50),
                
                Messages = get_messages(AdminClient),
                {user_list, Users, Admins} = lists:keyfind(user_list, 1, Messages),
                ?assert(lists:member("regular_user", Admins))
             end},
             
            {"Non-admin cannot make admin",                                         %% === Correct =====
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),
                Client3 = mock_client(),
                
                % Start with clean server to control admin status
                server:stop(),
                {ok, _} = server:start_link(10),
                
                {ok, _} = server:connect(Client1, "real_admin"),
                {ok, _} = server:connect(Client2, "fake_admin"),
                {ok, _} = server:connect(Client3, "target_user"),
                
                % Try to make user admin from non-admin account
                server:make_admin("fake_admin", "target_user"),
                timer:sleep(50),
                
                % Check the broadcast message
                Messages = get_messages(Client1),
                ?assert(lists:member({msg,"fake_admin is not an admin "},Messages))
             end},
             
            {"Remove admin",                                                        %% === Correct =====
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),
                
                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),
                
                {ok, _} = server:connect(Client1, "main_admin"),
                {ok, _} = server:connect(Client2, "temp_admin"),
                
                % Make user admin
                server:make_admin("main_admin", "temp_admin"),
                timer:sleep(50),
                
                % Remove admin status
                server:remove_admin("main_admin", "temp_admin"),
                timer:sleep(50),
                
                % Verify admin status
                server:request_user_list("main_admin"),
                timer:sleep(50),
                
                Messages = get_messages(Client1),
                {user_list, Users, Admins} = lists:keyfind(user_list, 1, Messages),
                ?assertNot(lists:member("temp_admin", Admins))
             end},
             
            {"Kick user",                                                           %% === Correct =====
             fun() ->
                AdminClient = mock_client(),
                UserClient = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),
                
                {ok, _} = server:connect(AdminClient, "kickmaster"),
                {ok, _} = server:connect(UserClient, "kick_target"),
                
                % Admin kicks user
                server:kick_user("kickmaster", "kick_target"),
                timer:sleep(50),
                
                % Verify user list doesn't contain kicked user
                server:request_user_list("kickmaster"),
                timer:sleep(50),
                
                Messages = get_messages(AdminClient),
                {user_list, Users, _Admins} = lists:keyfind(user_list, 1, Messages),
                ?assertNot(lists:member("kick_target", Users))
             end}
        ]
     end}.

topic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [
            {"Change topic",                                                        %% === Correct =====
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),
                
                {ok, _} = server:connect(Client1, "topic_changer"),
                {ok, _} = server:connect(Client2, "topic_viewer"),
                
                % Change topic
                server:change_topic("topic_changer", "New Discussion Topic"),
                timer:sleep(50),
                
                % Check broadcast message
                Messages1 = get_messages(Client1),
                Messages2 = get_messages(Client2),
                
                ?assert(lists:member({msg,"topic_changer changed the topic to: New Discussion Topic"},Messages2))
             end}
        ]
     end}.

user_list_and_history_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [
            {"Request user list",                                                   %% === Correct =====
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),
                
                {ok, _} = server:connect(Client1, "list_requester"),
                {ok, _} = server:connect(Client2, "another_user"),
                
                % Request user list
                ok = server:request_user_list("list_requester"),
                timer:sleep(50),
                
                Messages = get_messages(Client1),
            
                ?assert(lists:member({user_list,["another_user","list_requester"],["list_requester"]} , Messages))

             end},
             
            {"Get message history",                                                 %% === Correct =====
             fun() ->
                Client = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),

                {ok, _} = server:connect(Client, "history_user"),
                
                % Send some messages
                server:send_message("history_user", "First test message"),
                server:send_message("history_user", "Second test message"),
                timer:sleep(50),
                
                % Request history
                ok = server:get_message_history("history_user"),
                timer:sleep(50),
                
                Messages = get_messages(Client),

                ?assert(received_message_pattern(Messages, "history_user", "First test message")),
                ?assert(received_message_pattern(Messages, "history_user", "Second test message"))
             end}
        ]
     end}.

disconnect_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [
            {"Disconnect user",
             fun() ->
                Client1 = mock_client(),
                Client2 = mock_client(),

                % Start with clean server
                server:stop(),
                {ok, _} = server:start_link(10),

                {ok, _} = server:connect(Client1, "stayer"),
                {ok, _} = server:connect(Client2, "leaver"),
                
                % Disconnect user
                server:disconnect("leaver"),
                timer:sleep(50),
                
                % Check if broadcast happened
                Messages = get_messages(Client1),
                ?assert(lists:member({msg, "leaver has left the chat."}, Messages)),
                
                % Check user list
                server:request_user_list("stayer"),
                timer:sleep(50),
                
                UpdatedMessages = get_messages(Client1),
                {user_list, Users, _} = lists:keyfind(user_list, 1, UpdatedMessages),
                ?assertNot(lists:member("leaver", Users))
             end}
        ]
     end}.