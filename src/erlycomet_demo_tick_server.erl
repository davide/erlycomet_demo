%%%---------------------------------------------------------------------------------------
%%% @author     Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @copyright  2007 Roberto Saccon
%%% @doc        ErlyComet Demo Chat Application
%%% @reference  See <a href="http://erlycomet.googlecode.com" target="_top">http://erlycomet.googlecode.com</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon
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
-module(erlycomet_demo_tick_server).
-author('rsaccon@gmail.com').


%% Api
-export([start/0,
         stop/0]).


%% Internal exports
-export([tick/0]).

-include("erlycomet.hrl").

%%====================================================================
%% API functions
%%====================================================================
start() ->
	Clock = spawn(fun() -> ?MODULE:tick() end),
	register(clock, Clock),
	{ok, Clock}.
	

stop() ->
    clock ! stop.


%%====================================================================
%% Internal exports
%%====================================================================
tick() ->
    receive
	stop ->
	    ok
    after 1000 ->
	    {_,Secs,_} = now(),
        Channel = <<"/test/time">>,
        Data = Secs rem 1000,
        erlycomet_api:deliver_event(#event{channel=Channel, data=Data}),
        tick()
    end.

            
%%====================================================================
%% Internal functions
%%====================================================================
