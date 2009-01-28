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
-module(erlycomet_demo_mochiweb_server).
-author('rsaccon@gmail.com').


%% Api
-export([start/1,
         stop/0,
         stop/1]).


%% Internal exports
-export([loop/2, loop/4]).


%%====================================================================
%% API functions
%%====================================================================
start(Config) ->
	DocRoot = case proplists:get_value(docroot, Config, undefined) of
		undefined ->
			filename:join([filename:dirname(code:which(?MODULE)),"../priv/", "docroot"]);
		Dir -> Dir
	end,
	Loop = fun (Req) -> ?MODULE:loop(Req, DocRoot) end,
	Args = case proplists:get_value(port, Config, undefined) of
		undefined ->
			[{loop, Loop}];
	    Port ->
	        io:format("Mochiweb serving: ~p on Port: ~p ~n", [DocRoot, Port]),
			[{port, Port} | [{loop, Loop}]]
	end,
	mochiweb_http:start(Args).
	

stop() ->
    mochiweb_http:stop(?MODULE).


stop(Name) ->
    mochiweb_http:stop(Name).

            
%%====================================================================
%% Internal functions
%%====================================================================
loop(Req, DocRoot) ->
    loop(Req, Req:get(method), Req:get(path), DocRoot).

loop(Req, _, "/cometd", _) ->
	ErlyCometRequest = erlycomet_mochiweb_request:new(erlycomet_demo_rpc),
	ErlyCometRequest:handle(Req);
    	
loop(Req, 'GET', [$/ | Path], DocRoot) ->
    Req:serve_file(Path, DocRoot);
	
loop(Req, _Method, _Path, _) ->
	Req:not_found().
