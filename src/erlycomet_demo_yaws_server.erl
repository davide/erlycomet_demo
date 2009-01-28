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
-module(erlycomet_demo_yaws_server).
-author('nesrait@gmail.com').
-behaviour(gen_server).

-import(gen_server).
-import(application).
-import(yaws_config).
-import(lists).
-import(proplists).
-import(yaws_api).

-include_lib("yaws/include/yaws.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3, set_conf/1]).

-export([out/1]). % Yeap! This is also an appmod handler!
out(A) ->
	%~ % Just channels:
	%~ erlycomet_yaws_request:handle(A);
	% Channels + Support for RPC calls to the given module:
	ErlyCometRequest = erlycomet_yaws_request:new(erlycomet_demo_rpc),
	ErlyCometRequest:handle(A).

start_link(ServerConfigs) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ServerConfigs, []).

%% These ServerConfigs are carried here from the supervisor
init(ServerConfigs) ->
    process_flag(trap_exit, true),
    case application:start(yaws) of
        ok -> set_conf(ServerConfigs);
        Error -> {stop, Error}
    end.

set_conf(ServerConfigs) ->
    GC = yaws_config:make_default_gconf(false, "chat"),
    
    % override some default values
    GC1 = GC#gconf{logdir = "log"},
    % php_exe_path = <path to the php-cgi executable>
    % ebin_dir = <paths>
    % include_dir = <paths>
    % trace = http | traffic | false
    % copy_error_log = true
    % log_wrap_size = 1000000
    % log_resolve_hostname = false
    % fail_on_bind_err = true
    % auth_log = true
    % pick_first_virthost_on_nomatch = true
    
    % Override the server configs
    SCs = lists:map(
		fun(Sconf) ->
			#sconf{
					port = proplists:get_value(port, Sconf, 80),
					servername = proplists:get_value(servername, Sconf, "localhost"),
					listen = proplists:get_value(listen, Sconf, {0,0,0,0}),
					docroot = proplists:get_value(docroot, Sconf, "priv/www"),
					appmods = proplists:get_value(appmods, Sconf, []),
					opaque = proplists:get_value(opaque, Sconf, [])
			    }
		end, ServerConfigs),
    
    try yaws_api:setconf(GC1, [SCs]) of
        ok -> {ok, started};
        Errora -> {stop, Errora}
    catch
        Errorb -> {stop, Errorb}
    end.

handle_call(Request, _From, State) -> {stop, {unknown_call, Request}, State}.

handle_cast(_Message, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> application:stop(yaws), ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
