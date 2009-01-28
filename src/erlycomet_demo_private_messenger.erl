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
-module(erlycomet_demo_private_messenger).
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
	Id = "private_messenger",
	case erlycomet_api:subscribe(Id, <<"/service/privatechat">>) of
        ok ->
			Pid = spawn_link(fun() ->
										process_flag(trap_exit, true),
										?MODULE:loop()
									end),
			erlycomet_api:replace_server_connection(Id, Pid, connected),
			register(private_messenger, Pid),
			{ok, Pid};
        _ ->
			io:format("Error starting Private Messenger!~n"),
			{error, subscription_failed}
    end.

stop() ->
    private_messenger ! stop.


%%====================================================================
%% Internal exports
%%====================================================================
loop() ->
    receive
        {flush, Event} -> 
			pvt(Event),
			% TODO: stop message from reaching the remaining channel members
			% This would require that the server-side chat clients (logger, private_messenger, etc) would intervene directly
			% in the message delivery pipeline (non-existing at the moment).
			loop();
		stop ->
			io:format("Private Messenger terminating...~n"),
			terminate();
		{'EXIT', _, _} = Msg ->
			io:format("~nPrivate Messenger got an exit message: ~p. Terminating...~n", [Msg]),
			terminate();
        Other ->
			io:format("~nPrivate Messenger got message: ~p. Ignoring...~n", [Other]),
			loop()
    end.

            
%%====================================================================
%% Internal functions
%%====================================================================
pvt(#event{sender_id=SenderId, data={struct, [{<<"room">>, Channel}, {<<"chat">>, Msg}, {<<"peer">>, ToNickname}]}}) ->
	% TODO: this should return only one result... :)
	[ToClientId|_] = [Id || {Id, Nick} <- erlycomet_demo_chatroom_user:channel_users(Channel), Nick =:= ToNickname],
	[FromNickname|_] = [Nick || {Id, Nick} <- erlycomet_demo_chatroom_user:channel_users(Channel), Id =:= SenderId],
			
	PvtEvent = #event{channel=Channel, data={struct, [{<<"scope">>, <<"private">>}, {<<"user">>, FromNickname}, {<<"chat">>, Msg}]}},
	io:format("~p sent msg to ~p~n", [FromNickname, ToNickname]),
	erlycomet_api:deliver_to_connection(ToClientId, PvtEvent)
	;
pvt(Other) ->
	io:format("PVT no match!: ~p~n", [Other]),
	ok.

terminate() ->
	ok.
