-module(server).
-export([start/1,stop/1]).
-import(genserver, [start/3]).

% Record defining the state of the server, holding channels and users
-record(server_st, {
    channels,
    users % Pids paired with nicks
}).

-record(channel_st, {
    users % Only nicks
}).

% Function returning the initial state of the server, no channels nor servers
initial_state() ->
    #server_st{
        channels = [],
        users = []
    }.

initial_channel_state() ->
    #channel_st{
        users = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    genserver:start(ServerAtom, initial_state(), fun serverHandler/2).

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).

% Request from client to genserver, genserver uses this server which returns a new state and a response to the genserver which forwards to the client 

serverHandler(St, {join, Channel, Pid, Nick}) ->
    % Does the nick and Pid match? If so, we can use nicks for identification in-channel
    RegNick = inList(St#server_st.users, Nick),
    RegUser = RegNick andalso pidNick(St#server_st.users, Pid, Nick),

    %TODO: dumb fucking variables since you can't call functions from if statements, maybe replace with cases
    if 
        RegUser ->
            Users = St#server_st.users;
        not RegNick ->
            % add to list of users
            Users = [{Pid, Nick} |St#server_st.users];
        true -> 
            Users = St#server_st.users, % TODO: Will not be used, but otherwise we can't create a new State?
            {reply, pid_nick_mismatch, St}  % if Pid and Nick doesn't match, we return an error
    end,

    ChannelPid = list_to_atom(Channel),
    ChannelExist = inList(St#server_st.channels, list_to_atom(Channel)),
    if
        ChannelExist ->
            ChannelPid = list_to_atom(Channel),
            Channels = St#server_st.channels;
            % check if user is already in channel
        true ->
            % if not, create channel
            genserver:start(ChannelPid, initial_channel_state(), fun channelHandler/2),
            Channels = [ChannelPid|St#server_st.channels]
    end,

   % {_,Added,St} = genserver:request(ChannelPid, {addUser, Nick}),
    case genserver:request(ChannelPid, {addUser, Nick}) of
        nick_already_joined ->
            {reply, user_already_joined, St};
        _ ->
            UpdState = #server_st{
                channels = Channels,
                users = Users
            },
            {reply, ok, UpdState}

    end;

serverHandler(St, {leave, Channel, Nick, Pid}) ->
    not_implemented.



channelHandler(St, {addUser, Nick}) ->
    case inList(St#channel_st.users, Nick) of 
        true ->
            {reply, nick_already_joined, St};
        _ ->
            Users = [Nick | St#channel_st.users],
            State = St#channel_st{
                users = users
            },
            {reply, ok, State}
    end.


pidNick([{Pid, Nick}|_], Pid, Nick) -> true; % A match!
pidNick([_|Lst], Pid, Nick) -> pidNick(Lst, Pid, Nick); % Recursive call
pidNick(_,_,_) -> false. % This case means that the list is empty wo any Pid-Nick match, so false


% TODO: could be inList
inList([Channel|_], Channel) -> true;
inList([_|Lst], Channel) -> inList(Lst, Channel);
inList(_,_) -> false.
    
