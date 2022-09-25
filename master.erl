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
-define(WORK_UNIT, 250).
-define(NUM_OF_ACTORS, 30000).

%% API
-export([supervising_boss_actor/2, start_server_and_mine/2]).

%% Terminate all the slave workers, except master
terminate_workers(SlaveList) ->
  lists:foreach(
    fun(Workernode) ->
      {miner, Workernode} ! stop
    end,
    SlaveList
  ).

spawn_actors(WorkersList, K) ->
  % Select a random node from the existing list of workers
  Selected_node = rand:uniform(lists:flatlength(WorkersList)),

  %% Call spawn_link on that node, Args - which worker node?, which module?, which function?, what arguments?
  spawn_link(lists:nth(Selected_node, WorkersList), miner, mining_worker_actor, [self(), ?WORK_UNIT, K]).

%% For spawning the given number of actors, also spawn in other nodes if they are connected
create_mining_actors(WorkersList, 0, _) ->
  io:format("Completed spawning ~w actors in ~w worker nodes~n", [?NUM_OF_ACTORS, lists:flatlength(WorkersList)]),
  io:format("List of Workers = ~p~n", [WorkersList]);

create_mining_actors(WorkersList, Num_of_actors, K) ->
  case (Num_of_actors rem 50) == 0 of
    %% Check for any connect requests from worker clients, every 50 actor spawns and take action
    true ->
      receive
        {connect, Worker} ->
          io:format("One node connected~n"),
          %% Add the worker to the list of active workers, and continue spawning
          spawn_actors(lists:append(WorkersList, [Worker]), K),
          create_mining_actors(lists:append(WorkersList, [Worker]), Num_of_actors - 1, K)
      after 5 ->
      %% After 5ms, stop taking requests and continue spawning with the existing list of workers
      spawn_actors(WorkersList, K),
      create_mining_actors(WorkersList, Num_of_actors - 1, K)
      end;

    %% Just spawn normally on the existing WorkerList nodes, without waiting
    false ->
      spawn_actors(WorkersList, K),
      create_mining_actors(WorkersList, Num_of_actors - 1, K)
  end.


%% For listening to the messages from workers
bitcoin_receiver(Num_of_messages, Num_of_bitcoins) ->
  %% Wait until all the actors reply, by updating 'Num_of_messages' as we receive the messages
  case Num_of_messages > 0 of
    %% Check the type of message received
    true ->
      receive
        %% Print bitcoin, update the bitcoin count and wait for the next message in queue, if its a success
        {success, String_used_for_hash, Digest} ->
          io:format("~s ~s~n", [String_used_for_hash, Digest]),
          bitcoin_receiver(Num_of_messages - 1, Num_of_bitcoins + 1);

        % Just wait for the next message, if its a failure
        failure -> bitcoin_receiver(Num_of_messages - 1, Num_of_bitcoins)
      end;

    %% Print the number of bitcoins, actors were able to mine
    false -> io:format("Successfully mined ~w bitcoins~n", [Num_of_bitcoins])
  end.


%% Supervises all the spawned actors and listens to their messages
supervising_boss_actor(K, Masternode) ->
  % Start the timer for both cpu and real time
  erlang:statistics(runtime),
  erlang:statistics(wall_clock),

  %% Spawn the actors, using the current master node as the first worker
  create_mining_actors([Masternode], ?NUM_OF_ACTORS, K),

  %% Listen and print the bitcoins as they are received, Num_of_Actors = Num_of_messages to be received
  bitcoin_receiver(?NUM_OF_ACTORS, 0),

  %% Terminate all workers
  terminate_workers(nodes()),

% Stop the timer and calculate the performance
  {_, CPUTime} = erlang:statistics(runtime),
  {_, RealTime} = erlang:statistics(wall_clock),
  io:format("CPU time, Real time = ~w, ~w seconds~n", [CPUTime/1000, RealTime/1000]),
  io:format("Cores effectively used in mining = ~w~n", [CPUTime/RealTime]),

  %% For calculating the number of cores present in the system
  Cores_present_in_node = erlang:system_info(logical_processors_available),
  io:format("Number of cores ~w~n", [Cores_present_in_node]).


%% Start the server and accommodate other nodes as they become available, but mine independently if no workers connect
start_server_and_mine(K, ServerIP) ->
  %% Register the current process ID as module name - master
  erlang:register(?MODULE, self()),

  %% Name the current node as 'master' and Transform it into distributed node. Use longnames in server because clients use longnames
  Masternode = list_to_atom(string:concat("master@", ServerIP)),
  net_kernel:start([Masternode, longnames]),

  %% Set erlang cookie for the outgoing connections
  erlang:set_cookie(node(), mining_cluster),

  %% Start the boss actor to do mining independently
  master:supervising_boss_actor(K, Masternode),

  %% Stop being a distributed and exit cluster
  net_kernel:stop(),

  %% Unregister the master process
  erlang:unregister(?MODULE).
