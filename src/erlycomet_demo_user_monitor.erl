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
-module(erlycomet_demo_user_monitor).
-author('nesrait@gmail.com').


%% Api
-export([start_link/0,
         stop/0]).


%% Internal exports
-export([loop/0]).

-include("erlycomet.hrl").
-include("chatroom_user.hrl").

%%====================================================================
%% API functions
%%====================================================================
start_link() ->
	Id = "user_monitor",
	case erlycomet_api:subscribe(Id, <<"/chat/demo">>) of
        ok ->
			Pid = spawn_link(fun() ->
										process_flag(trap_exit, true),
										?MODULE:loop()
									end),
			erlycomet_api:replace_server_connection(Id, Pid, connected),
			register(user_monitor, Pid),
			{ok, Pid};
        _ ->
			io:format("Error starting user monitor!~n"),
			{error, subscription_failed}
    end.

stop() ->
    user_monitor ! stop.

%%====================================================================
%% Internal exports
%%====================================================================
loop() ->
    receive
        {flush, Event} -> 
            track_user(Event),
            loop();
		stop ->
			io:format("User Monitor terminating...~n"),
			terminate();
		{'EXIT', _, _} = Msg ->
			io:format("~nUser Monitor got an exit message: ~p. Terminating...~n", [Msg]),
			terminate();
        Other ->
			io:format("~nUser Monitor not ready to handle ~p. Terminating...~n", [Other]),
			terminate()
    end.
            
%%====================================================================
%% Internal functions
%%====================================================================
track_user(#event{channel=Channel, sender_id=SenderId, data={struct, Data}}) ->
	case lists:member({<<"join">>, true}, Data) of
		true ->
			Nickname = proplists:get_value(<<"user">>, Data),
			erlycomet_demo_chatroom_user:add({Channel, SenderId}, Nickname),
			Users = [Nick || {_ClientId, Nick} <- erlycomet_demo_chatroom_user:channel_users(Channel)],
			UsersEvent = #event{channel=Channel, data={struct, [{<<"members">>, Users}]}},
			io:format("~p as joined ~p!~n", [Nickname, Channel]),
			erlycomet_api:deliver_event(UsersEvent);
		_ ->
			case lists:member({<<"leave">>, true}, Data) of
				true ->
					Nickname = proplists:get_value(<<"user">>, Data),
					erlycomet_demo_chatroom_user:delete({Channel, SenderId}),
					io:format("~p as left ~p!~n", [Nickname, Channel]),
					Users = [Nick || {_ClientId, Nick} <- erlycomet_demo_chatroom_user:channel_users(Channel)],
					UsersEvent = #event{channel=Channel, data={struct, [{<<"members">>, Users}]}},
					erlycomet_api:deliver_event(UsersEvent);
				_ ->
					ok
			end
	end.

terminate() ->
	ok.
	

