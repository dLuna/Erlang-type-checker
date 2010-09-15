%% -*- erlang-indent-level: 2 -*-
%%=============================================================================
%% @author Daniel Luna <daniel@lunas.se>
%% @copyright 2007 Daniel Luna
%% 
%% @doc Different ways to start a contract system:
%%
%% 1. hack the code loader dynamically. contracts:start().
%% 2. change code loader at start.      erl -contracts
%% 3. compile files with contracts.     erlc -contracts
%%=============================================================================

-module(contracts).
-behaviour(gen_server).
%% gen_server exports
-export([start_link/0, start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).
-export([frequency/0, total/0, reset/0, pp/2]).

-export([core_transform/2]).
-export([verify_input/3, verify_output/3]).

%%----------------------------------------------------------------------------

-include_lib("dialyzer/src/dialyzer.hrl").	% for #contract{}

%%----------------------------------------------------------------------------

%% Temporary types -- fix these or add appropriate imports
-type(cerl()         :: any()).
-type(spec()         :: {atom() | tuple(), tuple() | list(), []} |
      {tuple(), list()}).
-type(contract()     :: any()).
-type(record_types() :: any()).

-type(c_opts()       :: [atom() | {atom(), _}]).

%%----------------------------------------------------------------------------

-spec(core_transform/2 :: (cerl(), c_opts()) -> record_types()).
core_transform(Code, _Opts) ->
  {Code1, _} = cerl_trees:label(cerl:from_records(Code)),
  Code2 = add_contracts(Code1),
  cerl:to_records(Code2).

-spec(add_contracts/1 :: (cerl()) -> cerl()).
add_contracts(Cerl0) ->
  Specs = lists:zf(fun is_spec/1, cerl:module_attrs(Cerl0)),
  Types = lists:zf(fun is_type/1, cerl:module_attrs(Cerl0)),
  TypeList = [{attribute, fixme, type, Type} || Type <- Types],
  M = cerl:atom_val(cerl:module_name(Cerl0)),
  {ok, Dict} = dialyzer_utils:get_record_and_type_info(TypeList),
  NewMDs = [insert_verification(MD, Specs, Dict, M)
	    || MD <- cerl:module_defs(Cerl0)],
  cerl:update_c_module(Cerl0,
		       cerl:module_name(Cerl0),
		       cerl:module_exports(Cerl0),
		       cerl:module_attrs(Cerl0),
		       NewMDs).

-spec(is_type/1 :: (cerl()) -> {'true', spec()} | 'false').
is_type(X) -> is(type, X).

-spec(is_spec/1 :: (cerl()) -> {'true', spec()} | 'false').
is_spec(X) -> is(spec, X).

-spec(is/2 :: ('spec' | 'type', {cerl(),cerl()}) -> {'true',spec()} | 'false').
is(Concrete, {Type, Data}) ->
  case (cerl:is_literal(Type) andalso cerl:concrete(Type) =:= Concrete) of
    true ->
      case cerl:concrete(Data) of
	[Spec] -> {true, Spec};
	_      -> false
      end;
    _    -> false
  end.

-spec(insert_verification/4 :: ({cerl(), cerl()}, [spec()], dict(), atom()) ->
 	 {cerl(), cerl()}).
insert_verification(C, _, _, ?MODULE) -> C;
insert_verification({CName, CFunction}, Specs, RecDict, M) ->
  Name = cerl:var_name(CName),
  {F, A} = Name,
  MFA  = {M, F, A},
  case lists:keysearch(Name, 1, Specs) of
    {value, {Name, Spec}} ->
      {CName, add_verification(CFunction, Spec, MFA, RecDict)};
    false ->
      {CName, CFunction}
  end.

add_verification(FunDef, Spec, Name, RecDict) ->
  log(adding_contract, Name),
  OrigBody  = cerl:fun_body(FunDef),
  Args      = cerl:fun_vars(FunDef),
  Contracts = dialyzer_contracts:contract_from_form(Spec, RecDict),
  Cs0       = Contracts#contract.contracts,
  Cs        = flatten_funs(Cs0),
  NewBody =
    cerl:c_call(cerl:c_atom(?MODULE),
		cerl:c_atom(verify_output),
		[cerl:c_call(cerl:c_atom(?MODULE),
			     cerl:c_atom(verify_input),
			     [cerl:make_data({atomic, Cs}, []),
			      cerl:c_tuple(Args),
			      cerl:make_data({atomic, Name}, [])]),
		 OrigBody,
		 cerl:make_data({atomic, Name}, [])]),
  NewFun = cerl:update_c_fun(FunDef, cerl:fun_vars(FunDef), NewBody),
  NewFun.

flatten_funs(Contracts) ->
  [{flatten_fun(T_Fun), C} || {T_Fun, C} <- Contracts].

flatten_fun(Contract) ->
  Domain = [flat_fun(C) || C <- erl_types:t_fun_args(Contract)],
  Range  = flat_fun(erl_types:t_fun_range(Contract)),
  erl_types:t_fun(Domain, Range).

flat_fun(T) ->
  erl_types:t_map(fun fix_fun/1, T).

fix_fun(T) ->
  case erl_types:t_is_fun(T) andalso
    not erl_types:t_is_any(erl_types:t_fun_arity(T)) of
    true -> erl_types:t_fun(erl_types:t_fun_arity(T), erl_types:t_any());
    false -> T
  end.

-spec(verify_input/3 :: ([contract()], tuple(any()), mfa()) -> [contract()]).
verify_input(Contracts, Args, {M,F,A}) ->
  log(in, {M, F, A}),
  %% error_logger:info_msg("~p:~p/~p~n", [M, F, A]),
  ArgList = tuple_to_list(Args),
  ArgTypes = [erl_types:t_from_term(Arg) || Arg <- ArgList],
  Match = [C || {T_Fun, _} = C <- Contracts,
		begin
		  Domain = erl_types:t_fun_args(T_Fun),
		  lists:all(fun({Arg, T}) ->
				erl_types:t_is_subtype(Arg, T) end,
			    lists:zip(ArgTypes, Domain))
		end],
  case Match of
    [] ->
      ArgStr = pp_args(ArgTypes),
      Domains = [erl_types:t_fun_args(T_Fun) || {T_Fun, _} <- Contracts],
      ContractStr = pp_domains(Domains),
      Msg = io_lib:format("~s\n  are not all subtypes of:\n    ~s\n",
			  [ArgStr, ContractStr]),
      ST = try exit(blah) catch _:_ -> erlang:get_stacktrace() end,
      M0 = checker_utils:decode_modname(M),
      StackTrace = checker_utils:decode_stacktrace(ST),
      error_logger:info_msg("Call violates the contract of ~p:~p/~p\n"
			    "  argument type(s):\n    ~s"
			    "  stacktrace:\n    ~p",
			    [M0, F, A, Msg, tl(StackTrace)]);
    _  -> ok
  end,
  Match.

pp_domains(List) -> sep([sep(to_string(L), pp_ind()) || L <- List], pp_or()).
pp_args(List)    -> sep(to_string(List), pp_ind()).
pp_range(List)   -> sep(to_string(List), pp_or()).

pp_or()  -> "\n  or\n    ".
pp_ind() -> "\n    ".

sep([], _) -> "";
sep(L, Sep) ->
  lists:foldl(fun(A, Acc) -> [Acc, Sep|A] end, hd(L), tl(L)).

to_string(List) ->
  [erl_types:t_to_string(T) || T <- List].

-spec(verify_output/3 :: ([contract()], any(), mfa()) -> any()).
verify_output([], Result, _) -> Result;
verify_output(Contracts, Result, Name) ->
  log(out, Name),
  ResultType = erl_types:t_from_term(Result),
  RangeTypes = [erl_types:t_fun_range(T_Fun) || {T_Fun, _C} <- Contracts],
  IsMatch = lists:any(fun(T) ->
			  erl_types:t_is_subtype(ResultType, T)
		      end, RangeTypes),
  case IsMatch of
    false ->
      RangeStr = pp_range(RangeTypes),
      ResultStr = erl_types:t_to_string(ResultType),
      Txt = io_lib:format("~s~n  is not a subtype of:\n    ~s\n",
			  [ResultStr, RangeStr]),
      ST = try exit(blah) catch _:_ -> erlang:get_stacktrace() end,
      {M, F, A} = Name,
      M0 = checker_utils:decode_modname(M),
      StackTrace = checker_utils:decode_stacktrace(ST),
      error_logger:info_msg("Return violates the contract of ~p:~p/~p:\n"
			    "  the term:\n    ~s"
			    "  stacktrace: ~p",
			    [M0, F, A, Txt, tl(StackTrace)]);
    true  -> ok
  end,
  Result.

-define(SERVER, ?MODULE).
log(Type, MFA) ->
  gen_server:cast(?SERVER, {Type, MFA}).
frequency() ->
  gen_server:call(?SERVER, frequency).
total() ->
  gen_server:call(?SERVER, total).
reset() ->
  gen_server:call(?SERVER, reset).

-record(state, {checked        = dict:new() :: dict()
		,with_contract = sets:new() :: set()
	       }).

-spec(start_link/0 :: () -> pid()).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(start/0 :: () -> {ok, pid()} | {error, any()}).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).

-spec(init/1 :: ([]) -> {'ok', #state{}}).
init([]) -> {ok, #state{}}.

-spec(handle_call/3 ::
      ('contracts', pid(), #state{}) -> {reply, list(), #state{}}).
handle_call(frequency, _From, S = #state{checked = Dict}) ->
  {reply, lists:sort(dict:to_list(Dict)), S};
handle_call(total, _From, S = #state{with_contract = Set}) ->
  {reply, lists:sort(sets:to_list(Set)), S};
handle_call(reset, _From, S) ->
  {reply, ok, S#state{checked = dict:new()}};
handle_call(_Msg, _From, S) ->
  error_logger:info_msg("~p: Unknown call ~p from ~p", [?MODULE, _Msg, _From]),
  {noreply, S}.

-spec handle_cast/2 :: ({'in', mfa()}, #state{}) -> {noreply, #state{}};
		       ({'out', mfa()}, #state{}) -> {noreply, #state{}}.
handle_cast({Type, MFA}, S = #state{checked = Dict0})
  when Type =:= in; Type =:= out ->
  Dict = dict:update_counter({Type, MFA}, 1, Dict0),
  {noreply, S#state{checked = Dict}};
handle_cast({adding_contract, MFA}, S = #state{with_contract = Set0}) ->
  Set = sets:add_element(MFA, Set0),
  {noreply, S#state{with_contract = Set}};
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

pp(FreqList, Total) ->
  [io_lib:format("~w:~w/~w checked ~w times.~n",
		 [checker_utils:decode_modname(M), F, A,
		  find_freq({M, F, A}, FreqList)])
   || {M, F, A} <- Total].

find_freq(MFA, FreqList) ->
  case lists:keysearch({in, MFA}, 1, FreqList) of
    {value, {{in, MFA}, Freq}} ->
	Freq;
     false ->
	0
    end.
