-module(server).
-export([start/1,stop/1]).
-import(genserver, [start/3]).
-import(lists, [member/2, delete/2]).

% Record defining the state of the server, holding channels and users
-record(server_st, {
    channels,
    users % Pids, then Nicks
}).

-record(channel_st, {
    name,
    users % Pids, then Nicks
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
    genserver:stop(ServerAtom).

% Request from client to genserver, genserver uses this server which returns a new state and a response to the genserver which forwards to the client 

serverHandler(St, {join, Channel, Pid, Nick}) ->
    % Does the nick and Pid match? If so, we can use nicks for identification in-channel
    RegNick = lists:member(Nick, St#server_st.users),
    RegUser = RegNick andalso pidNick(St#server_st.users, Pid, Nick),

    %TODO: dumb fucking variables since you can't call functions from if statements, maybe replace with cases
    %TODO: behövs det ens????
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
    case genserver:request(ChannelAtom, {addUser, {Pid, Nick}}) of
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
    %ChannelAtom = list_to_atom(Channel),
     %Check if Channel exists
    Server = not_yet_checked,
    case lists:member(ChannelAtom, St#server_st.channels) of
        true ->
            Server = genserver:request(ChannelAtom, {leave, User});
        _ ->
            % Return error message if Channel does not exist
            {reply, channel_does_not_exist, St}
    end,

    case Server of
        user_not_joined ->
            % Return error message if user is not member of this channel
            {reply, user_not_joined, St};
        _ ->
            % return ok. State does not need to be updated
            {reply, ok, St}
    end;

serverHandler(St, {channelExists, ChannelAtom}) ->
    {reply, lists:member(ChannelAtom, St#server_st.channels), St}.

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
channelHandler(St, {message_send, Msg, {Pid, Nick}}) ->
    case lists:member({Pid, Nick}, St#channel_st.users) of 
        true ->
            massMail(St#channel_st.users, Msg, Nick, St#channel_st.name),
            {reply, ok, St};
        _ ->
            {reply, user_not_joined, St}
    end.

% helper function to channelHandler for message_send
% recursively request genserver to send message to client's message_reseive handler
%TODO: unnecessary to message in different threads since the request format still means it waits for an answer and is blocked under its duration??
% TODO: jooo??
massMail([{Target, TNick}|Lst], Msg, Nick, Channel) ->
    if 
        TNick == Nick ->
            massMail(Lst, Msg, Nick, Channel);
        true ->
            genserver:request(Target, {message_receive, Channel, Nick, Msg}),
            massMail(Lst, Msg, Nick, Channel)
    end;
massMail(_,_,_,_) -> 
    ok.


% checks if Pid and Nick match
pidNick([{Pid, Nick}|_], Pid, Nick) -> true; % A match!
pidNick([_|Lst], Pid, Nick) -> pidNick(Lst, Pid, Nick); % Recursive call
pidNick(_,_,_) -> false. % This case means that the list is empty wo any Pid-Nick match, so false
