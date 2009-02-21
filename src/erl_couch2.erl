%%%-------------------------------------------------------------------
%%% File    : erl_couch2.erl
%%% Author  : Bob Dionne <>
%%% Description : 
%%%
%%% Created : 21 Feb 2009 by Bob Dionne <>
%%%-------------------------------------------------------------------
-module(erl_couch2).

-behaviour(gen_server).

-compile(debug_info).

-define(JSON_ENCODE(V), mochijson2:encode(V)).

-define(JSON_DECODE(V), mochijson2:decode(V)).

-ifdef(debug).

-define(DEBUG(Format, Args), io:format(Format, Args)).

-else.

-define(DEBUG(Format, Args), true).

-endif.


%% API
-export([start_link/1, all_dbs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Note to self: perhaps declare a record for state
%%
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).


all_dbs() ->
    gen_server:call(?MODULE, {all_dbs}).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Options]) ->
    ?DEBUG("inside init ~n",[]),
        RequestFunction = case proplists:get_value(ibrowse,
					       Options)
			  of
			  true ->
			      fun (Method, Data, Url, Args) ->
				      ibrowse_request(Method, Data, Url, Args)
			      end;
			  _ ->
			      fun (Method, Data, Url, Args) ->
				      inets_request(Method, Data, Url, Args)
			      end
		      end,
    HostUrl = couch_get_host_url(Options),
    {ok, {HostUrl, RequestFunction}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({all_dbs}, From, {Host, ReqFun}) ->
    ?DEBUG("ok Im called ~w ~n",[From]),
    Reply = couch_all_dbs(Host, ReqFun),
    {reply, Reply, {Host, ReqFun}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%% Request functions
couch_all_dbs(HostUrl, RequestFunction) ->
    ?DEBUG("~w",[HostUrl]),
    {Json, Raw} = RequestFunction(get, "", HostUrl ++ "/_all_dbs", []),
    process_result(Json, Raw).

process_result(Json, Raw) ->
    case Json of
	{[{<<"error">>, _},_]} -> {error, Json, Raw};
	_ -> {ok, Json}
    end.

inets_request(Method, Data, Url, Args) ->
    Host = couch_get_url(Url, Args),
    ?DEBUG("Requesting ~s~n", [Host]),
    {ok, {_Status, _Headers, Body}} = if Method =:= post;
					 Method =:= put ->
					      http:request(Method,
							   {Host,
							    [{"Content-Type",
							      "application/json"}],
							    [], Data},
							   [], []);
					 true ->
					      http:request(Method,
							   {Host,
							    [{"Content-Type",
							      "application/json"}]},
							   [], [])
				      end,
    ?DEBUG("received ~s ~n", [Body]),
    {?JSON_DECODE(Body), Body}.

ibrowse_request(Method, Data, Url, Args) ->
    Host = couch_get_url(Url, Args),
    ?DEBUG("Requesting ~s ~n", [Host]),
    ?DEBUG("Sending ~s ~n", [Data]),
    {ok, _Status, _Headers, Body} = if Method =:= post;
				       Method =:= put;
				       Method =:= delete ->
					    ibrowse:send_req(Host,
							     [{"Content-Type",
							       "application/json"}],
							     Method, Data);
				       true ->
					    ibrowse:send_req(Host,
							     [{"Content-Type",
							       "application/json"}],
							     Method)
				    end,
    ?DEBUG("received ~s ~n", [Body]),
    {?JSON_DECODE(Body), Body}.

couch_get_url(Url,[]) ->
    Url;

couch_get_url(Url,[H | T]) ->
    AddFirstArg = lists:append(Url,["?", element(1, H), "=", element(2, H)]),
    couch_get_rest_url(AddFirstArg,T).

couch_get_rest_url(Url, []) ->
    Url;

couch_get_rest_url(Url,[H, T]) ->
    AddArg = lists:append(Url,["&", element(1, H), "=", element(2, H)]), 
    couch_get_rest_url(AddArg,T).


couch_get_host_url(Options) ->
    Host = case proplists:get_value(host, Options) of
	       undefined -> "";
	       H -> H
	   end,
    Port = case proplists:get_value(port, Options) of
	       undefined -> "";
	       P -> P
	   end,
    URL = add_url_part(Host, "http://") ++
	add_url_part(Port, ":"),
    URL.


add_url_part(Part, Prefix) when is_integer(Part) ->
    add_url_part(integer_to_list(Part), Prefix);
add_url_part(Part, Prefix) ->
    case Part of
	"" -> "";
	_ -> lists:flatten([Prefix | Part])
    end.

