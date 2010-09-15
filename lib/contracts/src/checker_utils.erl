%% -*- erlang-indent-level: 2 -*-
%%=============================================================================
%% @author Kostis Sagonas <kostis@it.uu.se>, Daniel Luna <daniel@lunas.se>
%% @copyright 2008 Daniel Luna and Kostis Sagonas
%% 
%% @doc Provides utilities for the contract checker
%%=============================================================================

-module(checker_utils).
-export([encode_modname/1, decode_modname/1, decode_stacktrace/1,
	 spawn_modname/0, module_type/1]).
-export([get_core_from_beam/2]).

%%-----------------------------------------------------------------------------

-define(MAGIC_PREFIX, "checker_ m a g i c _").
-define(SPAWN_PREFIX, "checker_ s p a w n _").

%%----------------------------------------------------------------------------

-spec(encode_modname/1 :: (atom()) -> atom()).

encode_modname(M)
  when M =:= erlang;
       M =:= lists;
       M =:= prim_file;
       M =:= epp;
       M =:= erl_prim_loader -> M;
encode_modname(M) ->
  case atom_to_list(M) of
    ?MAGIC_PREFIX ++ _ -> M;
    ?SPAWN_PREFIX ++ _ -> M;
    Mod                -> list_to_atom(?MAGIC_PREFIX ++ Mod)
  end.

-spec(decode_modname/1 :: (atom()) -> atom()).

decode_modname(Mod) ->
  case atom_to_list(Mod) of
    ?MAGIC_PREFIX ++ M -> list_to_atom(M);
    ?SPAWN_PREFIX ++ M -> list_to_atom(M);
    _ -> Mod
  end.

-spec(spawn_modname/0 :: () -> atom()).
spawn_modname() ->
  list_to_atom(?SPAWN_PREFIX ++ "erlang").

-spec(decode_stacktrace/1 :: ([mfa()]) -> [mfa()]).

decode_stacktrace(StackTrace) ->
  [{decode_modname(M),F,A} || {M,F,A} <- StackTrace].

-spec(module_type/1 :: (atom()) -> checker | spawn | normal).
module_type(M) ->
  case atom_to_list(M) of
    ?MAGIC_PREFIX ++ _ -> checker;
    ?SPAWN_PREFIX ++ _ -> spawn;
    _ -> normal
  end.

%%----------------------------------------------------------------------------

-spec(get_core_from_beam/2 :: (string(), [_]) -> any()).
get_core_from_beam(File, Opts) ->
  NewOpts = [to_core, binary, report_errors, no_inline,
	     strict_record_tests, strict_record_updates],
  case beam_lib:chunks(File, [abstract_code]) of
    {ok, {_, List}} ->
      case lists:keysearch(abstract_code, 1, List) of
	{value, {abstract_code, {raw_abstract_v1, Abstr}}} ->
	  try compile:forms(Abstr, Opts ++ NewOpts) of
	      {ok, _, Core} -> Core;
	      _What -> error
	  catch
	    error:_Error -> error
	  end;
	_ -> error
      end;
    _ ->
      %% No or unsuitable abstract code.
      error
  end.
