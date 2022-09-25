%%%-------------------------------------------------------------------
%%% @author bhagyaraj
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Sep 2022 4:59 PM
%%%-------------------------------------------------------------------
-module('miner').
-author("bhagyaraj").
-define(RAND_SUFFIX_LEN, 10).

%% API
-export([mining_worker_actor/3, connect_to_server/2]).


mine_bitcoin(Server_PID, Workunit, K) ->
  %% Create the string to be hashed in format "gator_id;xxxxx"
  String_to_be_hashed = utils:gator_id_with_rand(?RAND_SUFFIX_LEN),

  %% Calculate sha256 digest for the generated string
  Digest = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, String_to_be_hashed))]),

  %% Separate the leftmost K digits in hash for comparison
  FirstKdigits = string:left(Digest, K),

  %% For creating a string of 'K' zeroes
  Kzeroes = lists:duplicate(K, $0),

  % Compare the first 'K' digits of the digest, if they are zero it is a bitcoin, else no
  case FirstKdigits == Kzeroes of
    %% Return success and the bitcoin found along with its string
    true -> Server_PID ! {success, String_to_be_hashed, Digest};

    %% Incase not matched, let actor continue until its work is done
    false ->
      case Workunit > 0 of
        %% Continue working if the job assignment is not yet done
        true -> mine_bitcoin(Server_PID, Workunit - 1, K);

        %% Return failure if bitcoin was not found
        false -> Server_PID ! failure
      end
  end.


%% Creating a different API, so the core mining logic is hidden to other modules
mining_worker_actor(Server_PID, Workunit, K) ->
  mine_bitcoin(Server_PID, Workunit, K).


%% Connect to server - 192.168.0.66
connect_to_server(ClientIP, ServerIP) ->
  %% Register the current process ID as module name - miner
  erlang:register(?MODULE, self()),

  %% Name the current node as 'slave' and Transform it into distributed node
  Slavenode = list_to_atom(string:concat("slave@", ClientIP)),
  net_kernel:start([Slavenode, longnames]),

  %% Set erlang cookie for the outgoing connections
  erlang:set_cookie(node(), mining_cluster),

  %% Connect to the server node using provided IP
  Masternode = list_to_atom(string:concat("master@", ServerIP)),
  net_kernel:connect_node(Masternode),

  %% Send the connect message to the master process
  {master, Masternode} ! {connect, Slavenode},

  %% wait for stop message from master
  receive
    stop -> done
  end,

  %% Stop being a distributed node and exit cluster
  net_kernel:stop(),

  %% Unregister the master process
  erlang:unregister(?MODULE).
