%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Sep 2022 3:33 PM
%%%-------------------------------------------------------------------
-module(master).
-author("bhagyaraj").
-define(WORK_UNIT, 1000).
-define(NUM_OF_ACTORS, 10000).

%% API
-export([supervising_boss_actor/1]).

%% For spawning the given number of actors
create_mining_workers(_, 0, _) ->
  io:format("Completed spawning ~w actors~n", [?NUM_OF_ACTORS]);
create_mining_workers(Boss_PID, Num_of_actors, K) ->
  %% Calling spawn using the following arguments respectively - miner module, mining_worker_actor function and it's arguments
  spawn(miner, mining_worker_actor, [Boss_PID, ?WORK_UNIT, K]),
  create_mining_workers(Boss_PID, Num_of_actors - 1, K).

%% For listening to the messages from workers
bitcoins_received(Num_of_messages, Num_of_bitcoins) ->
  %% Wait until all the actors reply, by updating 'Num_of_messages' as we receive the messages
  case Num_of_messages > 0 of
    %% Check the type of message received
    true ->
      receive
        %% Print bitcoin, update the bitcoin count and wait for the next message in queue, if its a success
        {success, String_used_for_hash, Digest} ->
          io:format("~s ~s~n", [String_used_for_hash, Digest]),
          bitcoins_received(Num_of_messages - 1, Num_of_bitcoins + 1);

        % Just wait for the next message, if its a failure
        failure -> bitcoins_received(Num_of_messages - 1, Num_of_bitcoins)
      end;

    %% Print the number of bitcoins, actors were able to mine
    false -> io:format("Successfully mined ~w bitcoins~n", [Num_of_bitcoins])
  end.

%% Supervises all the spawned actors and listens to their messages
supervising_boss_actor(K) ->
  %% For calculating the number of cores present in the system
  Cores_present_in_node = erlang:system_info(logical_processors_available),

  %% Spawn the workers
  create_mining_workers(self(), ?NUM_OF_ACTORS, K),

  %% Listen and print the bitcoins as they are received, Num_of_Actors = Num_of_messages to be received
  bitcoins_received(?NUM_OF_ACTORS, 0),

  io:format("Number of cores ~w~n", [Cores_present_in_node]).
