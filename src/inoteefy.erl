-module(inoteefy).
-behaviour (gen_server).

-export([
        init/1,
        terminate/2, 
        code_change/3,
        handle_call/3,
        handle_cast/2,
        handle_info/2
    ]).

-export([
        start_link/0,
        start/0,
        stop/0,
        watch/2,
        unwatch/1
    ]).


-record(state, {
            port, 
            fd,
            descriptors,
            files
        }).

-define(log(T),
        error_logger:info_report(
            [process_info(self(), current_function), {line,?LINE}, T])).

% the api

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() -> 
    gen_server:cast(?MODULE, stop).

watch(File,Callback) ->
    gen_server:cast(?MODULE, {watch, {File, Callback}}).

unwatch(File) -> 
    gen_server:cast(?MODULE, {unwatch, File}).

%% gen_server callbacks

init(_) ->
    DT = ets:new(descriptors, [private, set]),
    FT = ets:new(files, [private, set]),
    Port = open_port(),
    {ok, FD} = talk_to_port(Port, {open}),
    {ok, #state{port = Port, fd = FD, descriptors = DT, files = FT}}.

terminate(_, State) ->
    talk_to_port(State#state.port, {close, State#state.fd}).

code_change(_OstateVsn, State, _Extra) ->
    {ok, State}.

handle_info({Port, {data, Msg}}, State = #state{port = Port}) ->
    maybe_call_back(binary_to_term(Msg), State),
    {noreply, State};
handle_info(Msg, State) ->
    ?log({unknown_message, Msg}),
    {noreply, State}.

handle_call(Msg, _From, State) ->
    ?log({unknown_message, Msg}),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop,normal, State};
handle_cast({watch, Watch}, State) ->
    {noreply, do_watch(Watch, State)};
handle_cast({unwatch, Unwatch}, State) ->
    {noreply, do_unwatch(Unwatch, State)};
handle_cast(Msg, State) ->
    ?log({unknown_message, Msg}),
    {noreply, State}.

%% implementation details
open_port() ->
    Port = filename:join(priv_dir(), "inoteefy"),
    try
        open_port({spawn, Port}, [{packet, 2}, binary, exit_status])
    catch 
        _:_ -> exit({inotify_binary_not_found})
    end.

maybe_call_back({event, WatchDescriptor, Mask, Cookie, Name}, #state{descriptors = DT}) ->
    case ets:lookup(DT, WatchDescriptor) of
        [] ->
            case Mask of
                [ignored] -> ok;
                _ -> ?log([{got_event_without_callback, WatchDescriptor, Mask, Cookie, Name}])
            end;
        [{WatchDescriptor, {Filename,Callback}}] ->
            try Callback({Filename, Mask, Cookie, Name})
            catch C:R -> ?log([{callback_failed, Filename}, {C, R}])
            end
    end.

do_watch({File, Callback}, State) ->
    try {ok, WatchDescriptor} = talk_to_port(State#state.port, {add, State#state.fd, File, all}),
            ets:insert(State#state.files, {File, {WatchDescriptor, Callback}}),
            ets:insert(State#state.descriptors, {WatchDescriptor, {File, Callback}}),
            State
    catch C:R -> 
            ?log([{error_watching_file, File}, {C, R}]), State
    end.

do_unwatch(File, State) ->
    case ets:lookup(State#state.files, File) of
        [] ->
            ?log([{not_watching, File}]),
            State;
        [{File, {WatchDescriptor, _Callback}}] -> 
            try talk_to_port(State#state.port, {remove, State#state.fd, WatchDescriptor})
            catch C:R -> ?log([{error_unwatching_file, File}, {C, R}])
            end,
            ets:delete(State#state.files, File),
            ets:delete(State#state.descriptors, WatchDescriptor),
            State
    end.

talk_to_port(Port, Msg) ->
    try
        erlang:port_command(Port, term_to_binary(Msg)),
        receive 
            {Port, {data, D = <<131,104,2,_/binary>>}} -> binary_to_term(D)
        after 
            1000 -> throw(talk_to_port_timeout)
        end
    catch _:R -> throw({talking_to_port_failed, {R, Port, Msg}})
    end.

priv_dir() ->
    case code:priv_dir(inoteefy) of
        {error, bad_name} ->      
            Ebin = filename:dirname(code:which(?MODULE)),
            filename:join(filename:dirname(Ebin), "priv");
        Priv -> Priv
    end.
