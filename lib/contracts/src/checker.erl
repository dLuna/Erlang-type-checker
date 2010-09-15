%% -*- erlang-indent-level: 2 -*-
%%=============================================================================
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2008 Daniel Luna
%% 
%% @doc 
%% 
%%=============================================================================

-module(checker).
-behaviour(gen_server).
%% gen_server exports
-export([start_link/0, start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-export([run/3, run/4, run/1, run/2]).
-export([undefined_function/3]).

%% -define(debug, true).
-ifdef(debug).
-define(dbg(Format, Args), io:format(Format, Args)).
-else.
-define(dbg(F, A), ok).
-endif.

-define(SERVER, ?MODULE).

-record(state, {}).

-spec(start_link/0 :: () -> pid()).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(start/0 :: () -> {ok, pid()} | {error, any()}).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec(init/1 :: ([]) -> {'ok', #state{}}).
init([]) -> {ok, #state{}}.

-spec(handle_call/3 ::
      ({load, atom(), atom()}, pid(), #state{}) ->
	 {reply, {module | error, atom()}, #state{}};
      (stop, pid(), #state{}) -> {stop, ok, ok, #state{}};
      ('none()', any(), #state{}) -> no_return()).
handle_call({load, M, Origin}, _From, S) ->
  {reply, do_load_with_contracts(M, Origin), S};
handle_call(stop, _From, S) -> {stop, ok, ok, S};
handle_call(_Msg, _From, S) ->
  error_logger:info_msg("~p: Unknown call ~p from ~p", [?MODULE, _Msg, _From]),
  {noreply, S}.

-spec(handle_cast/2 :: ('none()', any()) -> no_return()).
handle_cast(_Msg, S) ->
  error_logger:info_msg("~p: Unknown cast ~p", [?MODULE, _Msg]),
  {noreply, S}.

-spec(handle_info/2 :: ('none()', any()) -> no_return()).
handle_info(_Info, S) -> {noreply, S}.

-spec(terminate/2 :: (_, #state{}) -> 'normal').
terminate(_Reason, #state{}) -> normal.

-spec(code_change/3 :: (any(), any(), any()) -> any()).
code_change(_OldVsn, S, _Extra) -> {ok, S}.

-spec(stop/0 :: () -> 'ok').
stop() -> gen_server:call(?SERVER, stop).

-spec(run/3 :: (atom(), atom(), [_]) -> any()).
run(M, F, Args      ) -> run(M, F, Args, []).

-spec(run/4 :: (atom(), atom(), [_], [_]) -> any()).
run(M, F, Args, Opts) -> run({M, F, Args}, Opts).

-spec(run/1 :: ({atom(), atom(), [_]} | [string()]) -> any()).
run({M, F, Args})     -> run({M, F, Args}, []);
%% From command line:
run([MStr, FStr, ArgsStr]) ->
  M = list_to_atom(MStr),
  F = list_to_atom(FStr),
  {ok, ItemTokens, _} = erl_scan:string(ArgsStr),
  {ok, Args} = erl_parse:parse_term(ItemTokens),
  %%   io:format("M: ~p, F: ~p, Args: ~p~n", [M, F, Args]),
  Res = try run({M, F, Args}, []) catch T:E -> {'EXIT', T,E} end,
  io:format("~p", [Res]).

-spec(run/2 :: ({atom(), atom(), [_]}, [_]) -> any()).
run({M, F, Args}, _Opts) ->
  case start() of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end,
  case contracts:start() of
    {ok, _} -> ok;
    {error, {already_started, _}} -> ok
  end,
  OldEH = process_flag(error_handler, ?MODULE),
  Res = try apply(checker_utils:encode_modname(M), F, Args)
	    after
	      process_flag(error_handler, OldEH)
	    end,
  FreqStr  = contracts:pp(contracts:frequency(), contracts:total()),
  error_logger:info_msg("~s~n", [FreqStr]),
  %%contracts:reset(),
  Res.

-spec(undefined_function/3 :: (atom(), atom(), [_]) -> any()).
undefined_function(Module, Func, Args) ->
  {module, checker_utils} = code:ensure_loaded(checker_utils),
  case checker_utils:module_type(Module) of
    spawn ->
      ?dbg("running ~p/~p (Args = ~p)~n", [Func, length(Args), Args]),
      case length(Args) of
	%% (Fun)
	1 -> [F] = Args, Fun = F, N = node();
	%% (Node, Fun)
	2 -> [N, F] = Args, Fun = F;
	%% (M, F, Ar)
	3 ->
	  [M, F, Ar] = Args,
	  Fun = fun() -> apply(checker_utils:encode_modname(M), F, Ar) end,
	  N = node();
	%% (N, M, F, Ar)
	4 ->
	  [N, M, F, Ar] = Args,
	  Fun = fun() -> apply(checker_utils:encode_modname(M), F, Ar) end
      end,
      erlang:Func(N,
		  fun() ->
		      process_flag(error_handler, ?MODULE),
		      Fun()
		  end);
    checker ->
      M = checker_utils:decode_modname(Module),
      case load_with_contracts(Module, M) of
	{module, Module} ->
	  apply(Module, Func, Args);
	{error, What} ->
	  error_logger:format("Error loading ~p (~p) (Error:~p)~n",
			      [Module, M, What]),
	  apply(M, Func, Args)
      end;
    normal ->
      error_handler:undefined_function(Module, Func, Args)
  end.

load_with_contracts(LoadName, Origin) ->
  gen_server:call(?SERVER, {load, LoadName, Origin}, 90000).

do_load_with_contracts(LoadName, Origin) ->
  {module, Origin} = code:ensure_loaded(Origin),
  case lists:member(debug_info,
		    hd([Opts || {options, Opts} <-
				  Origin:module_info(compile)])) of
    true ->
      case code:is_loaded(LoadName) of
	{file, _Loaded} -> {module, LoadName};
	false -> do_load_with_contracts_(LoadName, Origin)
      end;
    false ->
      error_logger:format("The module '~p' is not debug compiled; "
			  "cannot check contracts for this module~n",
			  [checker_utils:decode_modname(LoadName)]),
      {error, LoadName}
  end.

do_load_with_contracts_(LoadName, Origin) ->
  try 
    BeamPath = code:which(Origin),
    Core     = checker_utils:get_core_from_beam(BeamPath, []),
    NewCalls = set_core_name(LoadName, Core),
    NewCore  =
    try contracts:core_transform(NewCalls, [])
    catch error:Err ->
	%% Stupid erl_types has error types which are deep charlists
	ErrStr =
	  case is_list(Err) andalso
	    lists:all(fun(E) ->
			  is_integer(E) andalso 0 =< E andalso E =< 255
		      end,
		      lists:flatten(Err)) of
	    true  -> Err;
	    false -> io_lib:format("~w", [Err])
	  end,
	error_logger:info_msg("Failed to add contracts to ~w (error: ~s)~n"
			      "Stacktrace: ~p",
			      [checker_utils:decode_modname(LoadName), ErrStr,
			       erlang:get_stacktrace()]),
	NewCalls
    end,
    {ok, [], Bin, _}   = compile:forms(NewCore, [from_core, report, return]),
    case code:is_loaded(LoadName) of
      {file, _Loaded} -> {module, LoadName};
      false -> {module, LoadName} = code:load_binary(LoadName, BeamPath, Bin)
    end
  catch
    error:Error ->
      io:format("Error loading ~p! (~p)~n",
		[checker_utils:decode_modname(LoadName), Error]),
      io:format("stacktrace: ~p~n", [erlang:get_stacktrace()]),
      {error, LoadName}
  end.

set_core_name(Name, Code) ->
  {Cerl0, _} = cerl_trees:label(cerl:from_records(Code)),
  Cerl1 = cerl:update_c_module(Cerl0,
			       cerl:c_atom(Name),
			       cerl:module_exports(Cerl0),
			       cerl:module_attrs(Cerl0),
			       cerl:module_defs(Cerl0)),
  %% ?dbg("Creating ~p~n", [Name]),
  Cerl2 = update_function_calls(Cerl1),
  cerl:to_records(Cerl2).

update_function_calls(Core) ->
  %% io:format("~s~n", [cerl_prettypr:format(Core, [{noann, true}])]),
  cerl_trees:map(fun update_function_call/1, Core).

update_function_call(Core) ->
  case cerl:is_c_call(Core) of
    true ->
      M = cerl:call_module(Core),
      F = cerl:call_name(Core),
      case cerl:is_c_atom(M) andalso cerl:is_c_atom(F) of
	true ->
	  MAtom = cerl:atom_val(M),
	  FAtom = cerl:atom_val(F),
	  MFA   = {MAtom, FAtom, cerl:call_arity(Core)},
	  case erlang:is_builtin(MAtom, FAtom, cerl:call_arity(Core)) of
	    true ->
	      case MFA of
		{erlang, spawn,      3} -> patch_spawn(Core);
		{erlang, spawn_link, 3} -> patch_spawn(Core);
		_ -> Core
	      end;
	    false ->
	      case MFA of
		{erlang, spawn,         _} -> patch_spawn(Core);
		{erlang, spawn_link,    _} -> patch_spawn(Core);
		{erlang, spawn_monitor, _} -> patch_spawn(Core);
		_ -> patch_call(Core)
	      end
	  end;
	false ->
	  %% FIXME: When calling a module by variable, the variable
	  %% should be patched here:
	  Core
      end;
    false ->
      Core
  end.

patch_spawn(Call) ->
  patch(Call, checker_utils:spawn_modname()).

patch_call(Call) ->
  M = cerl:atom_val(cerl:call_module(Call)),
  NewM = checker_utils:encode_modname(M),
  patch(Call, NewM).

patch(Call, NewM) ->
  cerl:update_c_call(Call,
		     cerl:c_atom(NewM),
		     cerl:call_name(Call),
		     cerl:call_args(Call)).

