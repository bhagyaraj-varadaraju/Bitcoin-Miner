%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 3:31 PM
%%%-------------------------------------------------------------------
-module(utils).
-author("bhagyaraj").

%% API
-export([gator_id_with_rand/1]).

%% For prepending the random string with Gator link ID
gator_id_with_rand(L) ->
  Gatorlink = "varadaraju.b",
  string:concat(string:concat(Gatorlink, ";"), rand_alphanumeric(L)).

%% For defining the allowed characters in the random string
rand_alphanumeric(L) -> gen_rand(L, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890").

%% For generating the random string of given length
gen_rand(Length, AllowedChars) ->
  MaxLength = length(AllowedChars),
  lists:foldl(fun(_, Acc) ->
    [lists:nth(rand:uniform(MaxLength), AllowedChars)] ++ Acc end, [], lists:seq(1, Length)).
