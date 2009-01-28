%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author     Davide Marquês
%%% @copyright  2009 Roberto Saccon, Davide Marquês
%%% @doc        Helper module for easy application start, stop, reloading , etc.
%%% @reference  See <a href="http://github.com/davide/erlycomet_demo/tree" target="_top">http://github.com/davide/erlycomet_demo/tree</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Roberto Saccon, Davide Marquês
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(erlycomet_demo_sup).
-author('rsaccon@gmail.com').
-author('nesrait@gmail.com').

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1, upgrade/0]).

%% Definitions
-define(SERVER, ?MODULE).

-include("chatroom_user.hrl").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%% @end 
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).


%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.
	
	
%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end 
%%--------------------------------------------------------------------
init(Args) ->	
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxTimeBetweenRestarts = 10,
    SupFlags  = {RestartStrategy, MaxRestarts, MaxTimeBetweenRestarts},
	
	AdditionalMnesiaTables = [
			{chatroom_user, [{ram_copies, [node()]}, {attributes, record_info(fields, chatroom_user)}]}
		],
    ErlyComet = {erlycomet_cluster,
        {erlycomet_cluster, start, [AdditionalMnesiaTables]},
        permanent,
        1000,
        worker,
        [erlycomet_cluster]},

	TickServer = {erlycomet_demo_tick_server, 
	    {erlycomet_demo_tick_server, start, []},
        permanent,
        1000,
        worker,
        [erlycomet_demo_tick_server]},

	MochiwebConfig =
		case proplists:get_value(mochiweb_config, Args) of
			undefined ->
				exit("Missing mochiweb_config in the .app file!");
			MConf -> MConf
		end,
	MochiwebServer = {erlycomet_demo_mochiweb_server, 
	    {erlycomet_demo_mochiweb_server, start, [MochiwebConfig]},
        permanent,
        1000,
        worker,
        [erlycomet_demo_mochiweb_server]},
		
    YawsConfig =
		case proplists:get_value(yaws_config, Args) of
			undefined ->
				exit("Missing yaws_config in the .app file!");
			YConf -> YConf
		end,
	YawsServer = {erlycomet_demo_yaws_server, 
	    {erlycomet_demo_yaws_server, start_link, [YawsConfig]},
        permanent,
        1000,
        worker,
        [erlycomet_demo_yaws_server]},
		
    UserMonitor = {user_monitor,
        {erlycomet_demo_user_monitor, start_link, []},
        permanent,
        100,
        worker,
        [erlycomet_demo_user_monitor]},
	
	ConnectionsTimeout = 30,
	InactiveConnectionsMonitor =  {inactive_connections_monitor,
        {erlycomet_demo_inactive_connections_monitor, start_link, [ConnectionsTimeout]},
        permanent,
        100,
        worker,
        [erlycomet_demo_inactive_connections_monitor]},
	
	PrivateMessenger = {private_messenger,
        {erlycomet_demo_private_messenger, start_link, []},
        permanent,
        100,
        worker,
        [erlycomet_demo_private_messenger]},
	
    {ok,{SupFlags, [ErlyComet, TickServer, MochiwebServer, YawsServer, UserMonitor, InactiveConnectionsMonitor, PrivateMessenger]}}.


%%====================================================================
%% Internal functions
%%====================================================================