-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
    
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
    %ChannelAtom = list_to_atom(Channel),
    case is_process_alive(whereis(St#client_st.server)) of
        false ->
            {reply, {error, server_not_reached, "Server can not be reached"}, St};
        _ ->
            case genserver:request(St#client_st.server, {join, Channel, self(), St#client_st.nick}) of
                % Return, in message form, an Ok if performed, otherwise error with error message
                user_already_joined ->
                    {reply, {error, user_already_joined, "User has already joined this channel!"}, St};                    
                ok ->
                    % TODO: we return the arguement state unchanged since neither gui, nick nor server has changed ?
        %           UpdSt = St#client_st{
        %               channels = [ ChannelAtom | St#client_st.channels ]
        %            },
        %          {reply, ok, UpdSt} 
                    {reply, ok, St}%;
                %_ ->
                  %  {reply, {error, server_not_reached, "Server can not be reached"}, St}

            end
    end;
% Leave channel
handle(St, {leave, Channel}) ->

   ChannelAtom = list_to_atom(Channel),
   case is_process_alive(whereis(St#client_st.server)) of
        true ->
            ExistingChannel = genserver:request(St#client_st.server, {channelExists, ChannelAtom});
        _ ->
            ExistingChannel = false,
            {reply, {error, server_not_reached, "Server can not be reached"}, St}
    end,
%case lists:member(ChannelAtom, St#client_st.channels) of
    case ExistingChannel of
    %case genserver:request(St#client_st.server, {channelExists, ChannelAtom}) of
        true ->
            case genserver:request(ChannelAtom, {leave, self()}) of
                ok ->
                    {reply, ok, St};
                _ -> 
                    {reply,{error, user_not_joined, "User has not joined this channel"}, St}
                end;
        _ -> 
            {reply,{error, user_not_joined, "Channel does not exist"}, St}
        end;
%ChannelAtom = list_to_atom(Channel),
%case is_process_alive(whereis(ChannelAtom)) of
 %   true ->
  %      case genserver:request(ChannelAtom, {leave, ChannelAtom, {self(), St#client_st.nick}}) of 
   %         user_not_joined ->
    %            {reply, {error, user_not_joined, "User has not joined this channel"}, St};
     %       _ ->
      %          {reply, ok, St}
       % end;
    %_ ->
     %       {reply, {error, server_not_reached, "Server can not be reached"}, St}
%end;
        


% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    ChannelAtom = list_to_atom(Channel),
    %TODO: dont go through server

    try genserver:request(St#client_st.server, {channelExists, ChannelAtom}) of
        true ->
            MemberofChan = genserver:request(ChannelAtom, {message_send, Msg, self(), St#client_st.nick}),

            if 
                MemberofChan =:= ok ->
                    {reply, ok, St};
                true ->
                    {reply, {error, user_not_joined, "User has not joined this channel"}, St}
               
            % what is the result of the send request? if it's ok, send ok to gui. otherwise send error msg user_not_joined

            end;
        false ->
            {reply, {error, server_not_reached, "Unknown channel"}, St}
    
    catch 
        error:badarg -> {reply, {error, server_not_reached, "Server not reached"}, St};
        timeout_error -> {reply, {error, server_not_reached, "Server timedout"}, St}
    end;

    %MemberofChan = genserver:request(ChannelAtom, {message_send, Msg, self(), St#client_st.nick}),
    %ChanExists = is_process_alive(whereis(St#client_st.server)),
   
% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    %TODO: check for permission with server !!
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

%TODO: written as a task but seems to be done??
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
    genserver:request(St#client_st.server, {quit, self()}),
    %genserver:stop(self());
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
