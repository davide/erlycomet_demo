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
-module(erlycomet_demo_chatroom_user).
-author('nesrait@gmail.com').

-include("erlycomet.hrl").
-include("chatroom_user.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([
		 add/2,
         replace/2,
		 delete/1,
		 channel_users/1,
		 timeout_client_id_in_all_channels/1
		]).

%%====================================================================
%% API functions
%%====================================================================
%%-------------------------------------------------------------------------
%% @spec (string(), pid()) -> ok | error 
%% @doc
%% adds a chatroom_user
%% @end
%%-------------------------------------------------------------------------
add(ChatroomClientId, Nickname) -> 
    U = #chatroom_user{chatroom_client_id=ChatroomClientId, nickname=Nickname},
    F = fun() -> mnesia:write(U) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.
 
%%-------------------------------------------------------------------------
%% @spec (string(), string()) -> {ok, new} | {ok, replaced} | error 
%% @doc
%% replaces an chatroom_user
%% @end
%%-------------------------------------------------------------------------
replace(ChatroomClientId, Nickname) -> 
    U = #chatroom_user{chatroom_client_id=ChatroomClientId, nickname=Nickname},
    F1 = fun() -> mnesia:read({chatroom_user, ChatroomClientId}) end,
    {Status, F2} = case mnesia:transaction(F1) of
        {atomic, EA} ->
            case EA of
                [] ->
                    {new, fun() -> mnesia:write(U) end};
				[_Connection] ->
					{replaced, fun() -> mnesia:write(U) end}
            end;
        _ ->
            {new, fun() -> mnesia:write(U) end}
    end,
    case mnesia:transaction(F2) of
        {atomic, ok} -> {ok, Status};
        _ -> error
    end.

%%--------------------------------------------------------------------
%% @spec (string()) -> ok | error  
%% @doc
%% removes an chatroom_user
%% @end 
%%--------------------------------------------------------------------  
delete(ChatroomClientId) ->
    F = fun() -> mnesia:delete({chatroom_user, ChatroomClientId}) end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.

%%--------------------------------------------------------------------
%% @spec () -> list()
%% @doc
%% returns a list of chatroom_users for the given channel
%% @end 
%%--------------------------------------------------------------------
channel_users(Channel) ->
    do(qlc:q([ {ClientId, Nickname} ||
					#chatroom_user{chatroom_client_id={C, ClientId}, nickname=Nickname}
					<-mnesia:table(chatroom_user),
					C =:= Channel
				])).

%%--------------------------------------------------------------------
%% @spec (string()) -> ok | error  
%% @doc
%% removes all chatroom_users for the given client_id
%% Disclaimer: yeap I know this sucks :P
%% @end 
%%--------------------------------------------------------------------  
timeout_client_id_in_all_channels(ClientId) ->
	ChatRoomUsers =
		do(qlc:q([ {Channel, Nickname} ||
						#chatroom_user{chatroom_client_id={Channel, Id}, nickname=Nickname}
						<- mnesia:table(chatroom_user),
						Id =:= ClientId
					])),
	% Notify other users about timeout
	lists:foreach(
		fun({Channel, Nickname}) ->
			Event = #event{
							channel=Channel,
							sender_id=0,
							data={struct, [{<<"timeout">>, Nickname}]}
						},
			erlycomet_api:deliver_event(Event)
		end, 
		ChatRoomUsers),
	% Delete chatroom_users
    F = fun() ->
			lists:foreach(
				fun({Channel, _}) ->
					mnesia:delete({chatroom_user, {Channel, ClientId}})
				end,
				ChatRoomUsers)
		end,
    case mnesia:transaction(F) of
        {atomic, ok} -> ok;
        _ -> error
    end.

%%====================================================================
%% Internal exports
%%====================================================================
            
%%====================================================================
%% Internal functions
%%====================================================================
do(QLC) ->
    F = fun() -> qlc:e(QLC) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.
