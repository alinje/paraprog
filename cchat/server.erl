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
    %TODO: kill all channels
    genserver:request(ServerAtom, {stopChannels}),
    genserver:stop(ServerAtom).

% Request from client to genserver, genserver uses this server which returns a new state and a response to the genserver which forwards to the client 

serverHandler(St, {join, Channel, Pid, Nick}) ->
    % Does the nick and Pid match? If so, we can use nicks for identification in-channel
    RegNick = lists:member(Nick, St#server_st.users),
    RegUser = RegNick andalso lists:member(St#server_st.users, {Pid, Nick}),

    if 
        RegUser ->
            Users = St#server_st.users;
        not RegNick ->
            % add to list of users
            Users = [{Pid, Nick} |St#server_st.users];
        true -> 
            Users = St#server_st.users, % TODO: Will not be used, but otherwise we can't create a new State? really confusing exception error
            {reply, pid_nick_mismatch, St}  % if Pid and Nick doesn't match, we return an error
    end,

    %  Check if a channel with argument Channel name exists.
    ChannelAtom = list_to_atom(Channel),
    %ChannelExist = inList(St#server_st.channels, list_to_atom(Channel)),
    case lists:member(ChannelAtom, St#server_st.channels) of 
        true ->
            % channel already exists. Do nothing but assigning values to below variables
            % ChannelPid = list_to_atom(Channel),
            Channels = St#server_st.channels;
        _ ->
            % if not, create channel
            genserver:start(ChannelAtom, initial_channel_state(Channel), fun channelHandler/2),
            Channels = [ChannelAtom|St#server_st.channels]
    end,

    % request to channel process: addUser? 
    % Which adds the user if possible AND returns a boolean containing whether it was a success or not.
    case genserver:request(ChannelAtom, {addUser, Pid}) of
        nick_already_joined ->
            {reply, user_already_joined, St};
        _ ->
            UpdSt = #server_st{
                channels = Channels,
                users = Users
            },
            {reply, ok, UpdSt}

    end;

serverHandler(St, {leave, ChannelAtom, User}) ->
    %Check if Channel exists
    case lists:member(ChannelAtom, St#server_st.channels) of
        true ->
            case genserver:request(ChannelAtom, {leave, User}) of 
                user_not_joined ->
                    % Return error message if user is not member of this channel
                    {reply, user_not_joined, St};
                _ ->
                    % return ok. State does not need to be updated
                    {reply, ok, St}
            end;
        _ ->
            % Return error message if Channel does not exist
            {reply, channel_does_not_exist, St}
    end;


% Changes nick of user if not already taken
serverHandler(St, {nick, Pid, OldNick, NewNick}) ->
    NickExists =nickExists(St#server_st.users, NewNick),
    if
        OldNick == NewNick ->
            {reply, its_your_name, St};
        NickExists ->    
            {reply, nick_taken, St};
        true ->
            UpdSt = St#server_st{
                channels = St#server_st.channels,
                users = [{Pid, NewNick} | lists:delete({Pid, OldNick}, St#server_st.users)]
            },
            {reply, ok, UpdSt}
    end;

serverHandler(St, {channelExists, ChannelAtom}) ->
    {reply, lists:member(ChannelAtom, St#server_st.channels), St};

serverHandler(St, {quit, Pid}) ->
    NmbrChannels = length(St#server_st.channels),
    if  NmbrChannels =/= 0 ->
            Channels = lists:foreach(fun(Channel) -> genserver:request(Channel, {leave, Pid}) end, St#server_st.channels);
        true ->
            Channels = []  
    end,
    Users = removePid(St#server_st.users, Pid),
    UpdSt = St#server_st{
    channels = Channels,
        users = Users
    },
    {reply, ok, UpdSt};

serverHandler(St, {stopChannels}) ->
    {reply, lists:foreach(fun(Channel) -> genserver:stop(Channel) end, St#server_st.channels), St}.

    
% adds user to channel list of users
% if user has already joined, return error msg nick_already_joined
channelHandler(St, {addUser, User}) ->
    case lists:member(User, St#channel_st.users) of 
        true ->
            {reply, nick_already_joined, St};
        _ ->
            Users = [User | St#channel_st.users],
            UpdSt = St#channel_st{
                %TODO: definera kanalnamn???
                name = St#channel_st.name,
                users = Users
            },
            {reply, ok, UpdSt}
    end;

channelHandler(St, {leave, User}) ->
    case lists:member(User, St#channel_st.users) of
        true ->
            UpdSt = St#channel_st{
                name = St#channel_st.name,
                users = lists:delete(User, St#channel_st.users)
            },
            {reply, ok, UpdSt};
        _ ->
            {reply, user_not_joined, St}
    end;

% send message to all members of channel
channelHandler(St, {message_send, Msg, Pid, Nick}) ->
    case lists:member(Pid, St#channel_st.users) of 
        true ->
            massMail(St#channel_st.users, Msg, Nick, Pid, St#channel_st.name),
            {reply, ok, St};
        _ ->
            {reply, user_not_joined, St}
    end.

% helper function to channelHandler for message_send
% recursively request genserver to send message to client's message_reseive handler
%TODO: unnecessary to message in different threads since the request format still means it waits for an answer and is blocked under its duration??
% TODO: jooo??
massMail([Pid|Lst], Msg, Nick, SenderPid, Channel) ->
    if 
        SenderPid == Pid ->
            massMail(Lst, Msg, Nick, SenderPid, Channel);
        true ->
            genserver:request(Pid, {message_receive, Channel, Nick, Msg}),
            massMail(Lst, Msg, Nick, SenderPid, Channel)
    end;
massMail(_,_,_,_,_) -> 
    ok.

nickExists([{_, Nick}| _], Nick) -> true; % Nick exists among users in server
nickExists([_|Lst], Nick) -> nickExists(Lst, Nick); % Recursive call
nickExists(_,_) -> false. %Nick is unique in server

removePid([{Pid, _} | Lst], Pid) -> Lst; % Remove first item if it matches the pid
removePid([First|Last], Pid) -> [First|removePid(Last,Pid)]; % Match not found, continue with recursive call
removePid(_,_) -> []. % Match not found

