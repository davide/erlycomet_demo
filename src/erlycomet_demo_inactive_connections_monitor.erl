%%%---------------------------------------------------------------------------------------
%%% @author     Davide Marquês <nesrait@gmail.com>
%%% @copyright  2009 Davide Marquês
%%% @doc        ErlyComet Demo Chat Application
%%% @reference  See <a href="http://github.com/davide/erlycomet_demo/tree" target="_top">http://github.com/davide/erlycomet_demo/tree</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Davide Marquês
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
-module(erlycomet_demo_inactive_connections_monitor).
-author('nesrait@gmail.com').


%% Api
-export([start_link/1,
         stop/0]).


%% Internal exports
-export([loop/1]).

-include("chatroom_user.hrl").

%%====================================================================
%% API functions
%%====================================================================
start_link(Timeout) ->
	Id = "inactive_connections_monitor",
	Pid = spawn_link(fun() ->
									process_flag(trap_exit, true),
									?MODULE:loop(Timeout)
								end),
	erlycomet_api:replace_server_connection(Id, Pid, connected),
	register(inactive_connections_monitor, Pid),
	{ok, Pid}.

stop() ->
    inactive_connections_monitor ! stop.



%%====================================================================
%% Internal exports
%%====================================================================
loop(Timeout) ->
	case erlycomet_api:drop_inactive_connections(Timeout) of
		[] -> ok;
		TimedOutClientIds ->
			io:format("These fellas timed out: ~p!~n", [TimedOutClientIds]),
			lists:foreach(fun erlycomet_demo_chatroom_user:timeout_client_id_in_all_channels/1, TimedOutClientIds)
	end,
    receive
		stop ->
			io:format("Inactive Connections Monitor terminating...~n"),
			terminate();
		{'EXIT', _, _} = Msg ->
			io:format("~nInactive Connections Monitor got an exit message: ~p. Terminating...~n", [Msg]),
			terminate()
	after Timeout * 1000 ->
		loop(Timeout)
    end.
            
%%====================================================================
%% Internal functions
%%====================================================================

terminate() ->
	ok.
	

