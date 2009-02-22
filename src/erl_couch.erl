%% 
%%% @private
%%% File:      erl_couch.erl
%%% @author    Bob Dionne <> []
%%% @copyright 2009 Dionne Associates, LLC.
%%% @doc  
%%%
%%% @end 
%%% Created : 21 Feb 2009 by Bob Dionne <>
%%%-------------------------------------------------------------------
-module(erl_couch).

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
-export([start/1, stop/0, get_all_stats/0, get_stats/1, all_dbs/0, get_db/1, create_db/1, delete_db/1, all_docs/1, all_docs/2, get_doc/2, get_doc/3, save_doc/2, save_doc/3, save_doc/4, update_doc/3, update_doc/4, delete_doc/2]).

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
start(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

stop() ->
    gen_server:call(?MODULE, stop).


all_dbs() ->
    gen_server:call(?MODULE, {all_dbs}).

get_db(DbName) ->
    gen_server:call(?MODULE, {get_db, DbName}).

get_all_stats() ->
    gen_server:call(?MODULE, {all_stats}).

get_stats(Stats) ->
    gen_server:call(?MODULE, {stats, Stats}).

create_db(DbName) ->
    gen_server:call(?MODULE, {create_db, DbName}).

delete_db(DbName) ->
    gen_server:call(?MODULE, {delete_db, DbName}).

all_docs(DbName) ->
    all_docs(DbName, []).

all_docs(DbName, Args) ->
    get_doc(DbName, "_all_docs", Args).

get_doc(DbName, DocName) -> get_doc(DbName, DocName, []).

get_doc(DbName, DocName, Args) ->
    gen_server:call(?MODULE, {get_doc, DbName, DocName, Args}).

save_doc(DbName, Doc) ->
    gen_server:call(?MODULE, {save_doc, DbName, Doc}).

save_doc(DbName, DocName, Doc) ->
    save_doc(DbName, DocName, Doc, []).

save_doc(DbName, DocName, Doc, Args) ->
    gen_server:call(?MODULE, {save_doc, DbName, DocName, Doc, Args}).

update_doc(DbName, DocName, Doc) ->
    update_doc(DbName, DocName, Doc, []).

update_doc(DbName, DocName, Doc, Args) ->
    gen_server:call(?MODULE, {save_doc, DbName, DocName, Doc, Args}).

delete_doc(DbName, DocName) ->
    gen_server:call(?MODULE, {delete_doc, DbName, DocName}).

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
			      ibrowse:start(),
			      fun (Method, Data, Url, Args) ->
				      ibrowse_request(Method, Data, Url, Args)
			      end;
			  _ ->
			      inets:start(),
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
handle_call({all_dbs}, _, {Host, ReqFun}) ->
    Reply = couch_all_dbs(Host, ReqFun),
    {reply, Reply, {Host, ReqFun}};

handle_call({get_db, DbName}, _, {Host, ReqFun}) ->
    Reply = couch_get_db(Host, ReqFun, DbName),
    {reply, Reply, {Host, ReqFun}};

handle_call({all_stats}, _, {Host, ReqFun}) ->
    Reply = couch_stats(Host ++ "/_stats", ReqFun),
    {reply, Reply, {Host, ReqFun}};

handle_call({stats, Stats}, _, {Host, ReqFun}) ->
    Reply = couch_stats(Host ++ "/_stats" ++ Stats, ReqFun),
    {reply, Reply, {Host, ReqFun}};

handle_call({create_db, DbName}, _, {Host, ReqFun}) ->
    Reply = couch_create_or_delete_db(Host, ReqFun, DbName, put),
    {reply, Reply, {Host, ReqFun}};


handle_call({delete_db, DbName}, _, {Host, ReqFun}) ->
    Reply = couch_create_or_delete_db(Host, ReqFun, DbName, delete),
    {reply, Reply, {Host, ReqFun}};

handle_call({get_doc, DbName, DocName, Args}, _, {Host, ReqFun}) ->
    DocUrl = Host ++ add_url_part(DbName, "/") ++ add_url_part(DocName, "/"),
    Reply = couch_get_doc(DocUrl, ReqFun, Args),
    {reply, Reply, {Host, ReqFun}};

handle_call({save_doc, DbName, Doc}, _, {Host, ReqFun}) ->
    DocUrl = Host ++ add_url_part(DbName, "/"),
    Reply = couch_save_doc(DocUrl, ReqFun, Doc),
    {reply, Reply, {Host, ReqFun}};

handle_call({save_doc, DbName, DocName, Doc, Args}, _, {Host, ReqFun}) ->
    DocUrl = Host ++ add_url_part(DbName, "/") ++  add_url_part(DocName, "/"),
    Reply = couch_save_doc(DocUrl, ReqFun, Doc, Args, put),
    {reply, Reply, {Host, ReqFun}};

handle_call({delete_doc, DbName, DocName}, _, {Host, ReqFun}) ->
    DocUrl = Host ++ add_url_part(DbName, "/") ++  add_url_part(DocName, "/"),
    Reply = couch_delete_doc(DocUrl, ReqFun),
    {reply, Reply, {Host, ReqFun}};

handle_call(stop, _, {Host, ReqFun}) ->
    ReqFun(stop, [], Host, []),
    {stop, normal, stopped, {Host, ReqFun}}.


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
    ?DEBUG("stopping now ~n",[]),
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

couch_get_db(HostUrl, RequestFunction, DbName) ->
    DbUrl = HostUrl ++ add_url_part(DbName, "/"),
    {Json, Raw} = RequestFunction(get, [], DbUrl, []),
    process_result(Json, Raw).

couch_stats(HostUrl, RequestFunction) ->    
    {Json, Raw} = RequestFunction(get, "", HostUrl, []),
    process_result(Json, Raw).

couch_create_or_delete_db(HostUrl, RequestFunction, DbName, CreateOrDelete) ->
    DbUrl = HostUrl ++ add_url_part(DbName, "/"),
    ?DEBUG("trying to create ~s ~n",[DbUrl]),
    {Json, Raw} = RequestFunction(CreateOrDelete, "", DbUrl, []),
    process_result(Json, Raw).

couch_get_doc(DocUrl, RequestFunction, Args) ->
    {Json, Raw} = RequestFunction(get, [], DocUrl, Args),
    process_result(Json, Raw).
    

couch_save_doc(DocUrl, RequestFunction, Doc) ->
    JSONDoc = iolist_to_binary(?JSON_ENCODE(Doc)),
    {Json, Raw} = RequestFunction(post, JSONDoc, DocUrl, []),
    process_result(Json, Raw).

couch_save_doc(DocUrl, RequestFunction, Doc, Args, PostOrPut) ->
    JSONDoc = iolist_to_binary(?JSON_ENCODE(Doc)),
    {Json, Raw} = RequestFunction(PostOrPut, JSONDoc, DocUrl, Args),
    process_result(Json, Raw).

couch_delete_doc(DocUrl, RequestFunction) ->
    {Json, Raw} = RequestFunction(delete, [], DocUrl, []),
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
					 Method =:= put;
					 Method =:= delete ->
					      http:request(Method,
							   {Host,
							    [{"Content-Type",
							      "application/json"}],
							    [], Data},
							   [], []);
					 true -> if Method =:= stop ->
							 inets:stop(),
							 {ok, {ok, ok, "[\"ok\"]"}};
						    true ->
							 http:request(Method,
							   {Host,
							    [{"Content-Type",
							      "application/json"}]},
							   [], [])
						 end
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
					    if Method =:= stop ->
						    ibrowse:stop(),
						    {ok, ok, ok, "[\"ok\"]"};
					       true ->
						    ibrowse:send_req(Host,
							     [{"Content-Type",
							       "application/json"}],
							     Method)
					    end
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

