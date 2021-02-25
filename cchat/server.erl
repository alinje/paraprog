-module(server).
-export([start/1,stop/1]).
-import(genserver, [start/3]).
-import(lists, [member/2, delete/2]).

% Record defining the state of the server, holding channels and users
-record(server_st, {
    channels, % only atoms
    users % Pids, then Nicks
}).

-record(channel_st, {
    name, % string name of the channel
    users % only Pids
}).

% Function returning the initial state of the server, no channels nor servers
initial_state() ->
    #server_st{
        channels = [],
        users = []
    }.

initial_channel_state(Name) ->
    #channel_st{
        name = Name,
        users = []
    }.

%%%%%%%% Server related functions %%%%%%%%

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state(), fun serverHandler/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % Return ok
    genserver:request(ServerAtom, {stopChannels}),
    genserver:stop(ServerAtom).


% serverHandler functions are called "from" a client through a call to function in genserver
serverHandler(St, {join, Channel, Pid, Nick}) ->
    % Does the nick and Pid match a registrered user? Or are they a non-match?
    RegNick = nickExists(St#server_st.users, Nick),
    RegUser = lists:member({Pid, Nick}, St#server_st.users),

    if 
        RegUser ->
            Users = St#server_st.users;
        not RegNick ->
            % add to list of users
            Users = [{Pid, Nick} |St#server_st.users];
        true -> 
            % RegNick and not RegUser, aka. invalid arguments as Pid and Nick mismatch
            Users = St#server_st.users,
            {reply, pid_nick_mismatch, St}
    end,

    %  Check if a channel with argument Channel name exists.
    ChannelAtom = list_to_atom(Channel),
    case lists:member(ChannelAtom, St#server_st.channels) of 
        true ->
            Channels = St#server_st.channels;
        _ ->
            % if not, create channel. Channels use handler channelHandler/2
            genserver:start(ChannelAtom, initial_channel_state(Channel), fun channelHandler/2),
            Channels = [ChannelAtom|St#server_st.channels]
    end,


    % channelHandler for addUser adds the user if possible and return value specifies is it succeeded.
    case genserver:request(ChannelAtom, {addUser, Pid}) of
        nick_already_joined ->
            {reply, user_already_joined, St};
        _ ->
            % if addUser succeeded we update St and return ok
            UpdSt = #server_st{
                channels = Channels,
                users = Users
            },
            {reply, ok, UpdSt}

    end;

% Changes nick of user if not already taken
serverHandler(St, {nick, Pid, OldNick, NewNick}) ->
    NickExists =nickExists(St#server_st.users, NewNick),
    if
        OldNick == NewNick ->
            % User should not be able to "change" to their current Nick
            {reply, its_your_name, St};
        NickExists ->
            % Nick is already taken
            {reply, nick_taken, St};
        true ->
            % Nick is free, the state is updated and the function return ok
            UpdSt = St#server_st{
                users = [{Pid, NewNick} | lists:delete({Pid, OldNick}, St#server_st.users)]
            },
            {reply, ok, UpdSt}
    end;

% Client shutdown. Removes client Pid from all channels
serverHandler(St, {quit, Pid}) ->
    % For each of the servers channels the Pid leaves the channel
    lists:foreach(fun(Channel) -> genserver:request(Channel, {leave, Pid}) end, St#server_st.channels),
    % Pid and corresponding nick is removed from the list of users
    Users = removePid(St#server_st.users, Pid),
    % State is updated
    UpdSt = St#server_st{
        users = Users
    },
    {reply, ok, UpdSt};

% Stops all of the server's channels
serverHandler(St, {stopChannels}) ->
    {reply, lists:foreach(fun(Channel) -> genserver:stop(Channel) end, St#server_st.channels), St}.

%%%%%%%% Channel related functions %%%%%%%%

% Adds pid to channel list of client pids
% if pid has already joined, return nick_already_joined
channelHandler(St, {addUser, Pid}) ->
    case lists:member(Pid, St#channel_st.users) of 
        true ->
            {reply, nick_already_joined, St};
        _ ->
            Users = [Pid | St#channel_st.users],
            UpdSt = St#channel_st{
                users = Users
            },
            {reply, ok, UpdSt}
    end;

% Client identified by pid leaves channel
% If pid was never member of this channel, user_not_joined is returned
channelHandler(St, {leave, Pid}) ->
    case lists:member(Pid, St#channel_st.users) of
        true ->
            UpdSt = St#channel_st{
                name = St#channel_st.name,
                users = lists:delete(Pid, St#channel_st.users)
            },
            {reply, ok, UpdSt};
        _ ->
            {reply, user_not_joined, St}
    end;

% Send message to all members of channel
% If sender pid is not a member of this channel, user_not_joined is returned
channelHandler(St, {message_send, Msg, Pid, Nick}) ->
    case lists:member(Pid, St#channel_st.users) of 
        true ->
            % The message sending itself needs no contact with the channel
            % We spawn a new process for sending the messages, to free the channel
            spawn(fun()->  massMail(St#channel_st.users, Msg, Nick, Pid, St#channel_st.name) end),
            {reply, ok, St};
        _ ->
            {reply, user_not_joined, St}
    end.

% Send message from SenderPid to all members of list, from a given channel and nick
% Recursively requests genserver to send message to client's message_receive handler
massMail(AllRec=[Pid|Lst], Msg, Nick, SenderPid, Channel) ->
    Branch = length(AllRec),
    if
        % If the length of the member list is very long, it is split into two processes to quicken the sending
        % 100 is a arbitrarily picked number, can be optimized through more testing
        Branch > 100 ->
            {Top, Bot} = lists:split(Branch/2, AllRec),
            spawn(fun()-> massMail(Top, Msg, Nick, SenderPid, Channel) end),
            spawn(fun()-> massMail(Bot, Msg, Nick, SenderPid, Channel) end);
        % The message is not sent to the sender itself
        SenderPid == Pid ->
            massMail(Lst, Msg, Nick, SenderPid, Channel);
        true ->
            % Send a request to genserver for client's message_receive
            genserver:request(Pid, {message_receive, Channel, Nick, Msg}),
            % Recursive call for sending next message
            massMail(Lst, Msg, Nick, SenderPid, Channel)
    end;
% This pattern will match when there are no more recipients in the list
massMail(_,_,_,_,_) -> 
    ok.

%%%%%%%% Map helper functions %%%%%%%%
nickExists([{_, Nick}| _], Nick) -> true; % Nick exists among users in server
nickExists([_|Lst], Nick) -> nickExists(Lst, Nick); % Recursive call
nickExists(_,_) -> false. %Nick is unique in server

removePid([{Pid, _} | Lst], Pid) -> Lst; % Remove first item if it matches the pid
removePid([First|Last], Pid) -> [First|removePid(Last,Pid)]; % Match not found, continue with recursive call
removePid(_,_) -> []. % Match not found
