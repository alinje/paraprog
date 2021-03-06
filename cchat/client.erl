-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.


% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel
handle(St, {join, Channel}) ->
    % Send request to genserver from state St
    % The data that will reach the handler in server is the data inside the curly brackets

    % Try to connect to the server and join a channel
    try genserver:request(St#client_st.server, {join, Channel, self(), St#client_st.nick}) of
        % Check value from the server and act according to the result message
        user_already_joined ->
            {reply, {error, user_already_joined, "User has already joined this channel!"}, St};                    
        ok ->
            {reply, ok, St}

    % Catch errors when connecting to the server
    catch 
        error:badarg -> {reply, {error, server_not_reached, "Server not reached"}, St};
        _:_ -> {reply, {error, server_not_reached, "Server timedout"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    ChannelAtom = list_to_atom(Channel),
    try genserver:request(ChannelAtom, {leave, self()}) of
        user_not_joined ->
            {reply,{error, user_not_joined, "User has not joined this channel"}, St};
        ok ->
            {reply, ok, St}    
    catch
        error:badarg -> {reply, {error, server_not_reached, "Server not reached"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    ChannelAtom = list_to_atom(Channel),
    try genserver:request(ChannelAtom, {message_send, Msg, self(), St#client_st.nick}) of
        user_not_joined ->
            {reply, {error, user_not_joined, "User has not joined this channel"}, St};
        ok ->
            {reply, ok, St}
    catch
        error:badarg -> {reply, {error, server_not_reached, "Server not reached"}, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    case genserver:request(St#client_st.server, {nick, self(), St#client_st.nick, NewNick}) of
        nick_taken -> 
            {reply, {error, nick_taken, "Nick is already taken"}, St};
        its_your_name->
            {reply, {error, its_your_name, "You already have this nick"}, St};
        _ ->
            {reply, ok, St#client_st{nick = NewNick}}
    end;
    
% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    % Call for the server to remove client from channels
    try genserver:request(St#client_st.server, {quit, self()}) of
        ok ->
            {reply, ok, St} 
    catch
        _:_ -> {reply, quit_was_unsuccessful, St}
    end;
    
% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
