%%% @copyright Copyright (c) 2007, Dmitrii 'Mamut' Dimandt.  All Rights Reserved.
%%%
%%% @doc
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% @end

%% @type json_object() = {json_object, proplist()}
%% @type raw_json() = string().
%%       Contains raw JSON string returned by CouchDB
%%
-module(erl_couch).

-compile(debug_info).

-define(JSON_ENCODE(V), mochijson2:encode(V)).

-define(JSON_DECODE(V), mochijson2:decode(V)).

-ifdef(debug).

-define(DEBUG(Format, Args), io:format(Format, [Args])).

-else.

-define(DEBUG(Format, Args), true).

-endif.

-export([test/0]).

-export([get_host/1, get_stats/1]).

-export([create_db/2, delete_db/2, get_db/2]).

-export([delete_doc/2, get_doc/2, get_doc/3, save_doc/2, save_doc/3, save_doc/4, update_doc/3, update_doc/4]).

-export([all_dbs/1, all_docs/1, all_docs/2]).

-export([create_view/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  PUBLIC API                                                              %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec get_host(Options::OptionList) -> Host::pid()
%% where
%%       OptionList = [Option]
%%       Option = {host, string()} | {port, integer()} | {ibrowse, true}
%%
%% @doc
%%       Retrievs a CouchDB host.
%%
%%       Required options: host, port
%%
%%       If you specify {ibrowse, true}, ibrowse will be used instead of inets
%% @end
get_host(Options) ->
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
    Pid = spawn(fun () -> host_loop(HostUrl,RequestFunction) end),
    Pid.

%% @spec all_dbs(Host::pid()) -> {DatabaseList, raw_json()}
%% where
%%       DatabaseList = tuple()
%%
%% @doc
%%
%%       Retrieves all dbs on the host
%% @end

all_dbs(Host) ->
    Host ! {self(), {all_dbs}},
    receive_and_return().

%% @spec get_db(Host::pid(), DBName::string()) -> DB::pid() | {error, json_object(), raw_json()}
%%
%% @doc Retrieves specified db from the host

get_db(Host, DBName) ->
    Host ! {self(), {get_db, DBName}},
    receive_and_return().

get_stats(Host) ->
    Host ! {self(), {stats}},
    receive_and_return().

%% @spec create_db(Host::pid(), DBName::string()) -> DB::pid() | {error, json_object(), raw_json()}
%%
%% @doc Creates specified db on the host

create_db(Host, DBName) ->
    Host ! {self(), {create_db, DBName}},
    receive_and_return().

%% @spec delete_db(Host::pid(), DBName::string()) -> DB::pid() | {error, json_object(), raw_json()}
%%
%% @doc Deletes specified db on the host

delete_db(Host, DBName) ->
    Host ! {self(), {delete_db, DBName}},
    receive_and_return().

%% @spec all_docs(DB::pid()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc Retrieves info on all docs available on the host
%% @equiv get_doc(DB, "_all_docs")

all_docs(DB) -> all_docs(DB, []).

all_docs(DB, Args) ->
    get_doc(DB, "_all_docs", Args).

%% @spec get_doc(DB::pid(), DocName::string()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc Retrieves the entire contents of the specified document

get_doc(DB, DocName) -> get_doc(DB, DocName, []).

get_doc(DB, DocName, Args) ->
    DB ! {self(), {get_doc, DocName, Args}},
    receive_and_return().

%% @spec get_doc_rev(DB::pid(), DocName::string(), Rev::integer()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @todo NOT IMPLEMENTED YET
%%
%% @doc
%%    Retrieves the entire contents of the specified document for the specified revision
%% @end


%% @spec save_doc(DB::pid(), Doc::proplist()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Saves a doc to the database. The doc is assigned a server-generated ID
%% @end
save_doc(DB, Doc) ->
    DB ! {self(), {save_doc, Doc}}, 
    receive_and_return().

%% @spec save_doc(DB::pid(), DocName::string(), Doc::proplist()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Saves a doc to the database with a user-supplied ID
%% @end
save_doc(DB, DocName, Doc) ->
    save_doc(DB, DocName, Doc, []).

save_doc(DB, DocName, Doc, Args) ->
    DB ! {self(), {save_doc, DocName, Doc, Args}},
    receive_and_return().

%% @spec update_doc(DB::pid(), DocName::string(), Rev::integer(), Doc::proplist()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Save a doc when the doc's revision is known
%% @end

update_doc(DB, DocName, Doc) ->
    update_doc(DB, DocName, Doc, []).

update_doc(DB, DocName, Doc, Args) ->
    DB ! {self(), {update_doc, DocName, Doc, Args}},
    receive_and_return().

%% @spec delete_doc(DB::pid(), DocName::string()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Delete a doc from the database
%% @end

delete_doc(DB, DocName) ->
    DB ! {self(), {delete_doc, DocName}},
    receive_and_return().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS                                                      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

host_loop(HostUrl, RequestFunction) ->
    receive
	{Pid, {create_db, DBName}} ->
	    Reply = couch_create_db(HostUrl, RequestFunction, DBName),
	    Pid ! Reply,
	    host_loop(HostUrl, RequestFunction);
	{Pid, {get_db, DBName}} ->
	    Reply = couch_get_db(HostUrl, RequestFunction, DBName),
	    Pid ! Reply,
	    host_loop(HostUrl, RequestFunction);
	{Pid, {all_dbs}} ->
	    Reply = couch_all_dbs(HostUrl ++ "/_all_dbs", RequestFunction),
	    Pid ! Reply,
	    host_loop(HostUrl, RequestFunction);
	{Pid, {stats}} ->
	    Reply = couch_stats(HostUrl ++ "/_stats", RequestFunction),
	    Pid ! Reply,
	    host_loop(HostUrl, RequestFunction);
	{Pid, {delete_db, DBName}} ->
	    Reply = couch_delete_db(HostUrl, RequestFunction, DBName),
	    Pid ! Reply,
	    host_loop(HostUrl, RequestFunction)
    end.

database_loop(DbUrl, RequestFunction) ->
    receive
	{Pid, {get_doc, DocName, Args}} ->
	    DocUrl = DbUrl ++ add_url_part(DocName, "/"),
	    Reply = couch_get_doc(DocUrl, RequestFunction, Args),
	    Pid ! Reply,
	    database_loop(DbUrl, RequestFunction);
	{Pid, {save_doc, Doc}} ->
	    Reply = couch_save_doc(DbUrl, RequestFunction, Doc),
	    Pid ! Reply,
	    database_loop(DbUrl, RequestFunction);
	{Pid, {save_doc, DocName, Doc, Args}} ->
	    DocUrl = DbUrl ++ add_url_part(DocName, "/"),
	    Reply = couch_save_doc(DocUrl, RequestFunction, Doc, Args, put),
	    Pid ! Reply,
	    database_loop(DbUrl, RequestFunction);
	{Pid, {update_doc, DocName, Doc, Args}} ->
	    DocUrl = DbUrl ++ add_url_part(DocName, "/"),
	    Reply = couch_save_doc(DocUrl, RequestFunction, Doc, Args, put),
	    Pid ! Reply,
	    database_loop(DbUrl, RequestFunction);
	{Pid, {delete_doc, DocName}} ->
	    DocUrl = DbUrl ++ add_url_part(DocName, "/"),
	    Reply = couch_delete_doc(DocUrl, RequestFunction),
	    Pid ! Reply,
	    database_loop(DbUrl, RequestFunction)
    end.

%% Internal functions

couch_create_db(HostUrl, RequestFunction, DbName) ->
    DbUrl = HostUrl ++ add_url_part(DbName, "/"),
    {Json, Raw} = RequestFunction(put, "", DbUrl, []),
    case Json of
	{[{<<"error">>, _},_]} -> {error, Json, Raw};
	_ ->
	    Pid = spawn(fun () -> database_loop(DbUrl, RequestFunction) end), Pid
    end.

couch_delete_db(HostUrl, RequestFunction, DbName) ->
    DbUrl = HostUrl ++ add_url_part(DbName, "/"),
    {Json, Raw} = RequestFunction(delete, "", DbUrl, []),
    process_result(Json, Raw).

couch_all_dbs(HostUrl, RequestFunction) ->    
    {Json, Raw} = RequestFunction(get, "", HostUrl, []),
    process_result(Json, Raw).

couch_stats(HostUrl, RequestFunction) ->    
    {Json, Raw} = RequestFunction(get, "", HostUrl, []),
    process_result(Json, Raw).

couch_get_db(HostUrl, RequestFunction, DbName) ->
    DbUrl = HostUrl ++ add_url_part(DbName, "/"),
    {Json, Raw} = RequestFunction(get, [], DbUrl, []),
    case Json of
	{[{<<"error">>, _},_]} -> {error, Json, Raw};
	_ ->
	    Pid = spawn(fun () -> database_loop(DbUrl, RequestFunction) end), Pid
    end.

couch_get_doc(DocUrl, RequestFunction, Args) ->
    {Json, Raw} = RequestFunction(get, [], DocUrl, Args),
    process_result(Json, Raw).

    

couch_save_doc(DbUrl, RequestFunction, Doc) ->
    JSONDoc = iolist_to_binary(?JSON_ENCODE(Doc)),
    {Json, Raw} = RequestFunction(post, JSONDoc, DbUrl, []),
    process_result(Json, Raw).

couch_save_doc(DocUrl, RequestFunction, Doc, Args, PostOrPut) ->
    JSONDoc = iolist_to_binary(?JSON_ENCODE(Doc)),
    {Json, Raw} = RequestFunction(PostOrPut, JSONDoc, DocUrl, Args),
    process_result(Json, Raw).

couch_delete_doc(DocUrl, RequestFunction) ->
    {Json, Raw} = RequestFunction(delete, [], DocUrl, []),
    process_result(Json, Raw).

%% Request functions
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

%% Utility functions
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

receive_and_return() ->
    receive
	Response -> Response after 5000 -> {error, timeout}
			     end.
%%
%%
process_result(Json, Raw) ->
    case Json of
	{[{<<"error">>, _},_]} -> {error, Json, Raw};
	_ -> {ok, Json}
    end.


%%
%% This test assumes a couchdb server running locally
%%
test() ->
    Host = get_host([{host, "127.0.0.1"}, {port, 5984},
		     {ibrowse, true}]),
    Db = create_db(Host, "erl-couch"),
    save_doc(Db,{[{<<"foo">>, <<"bar">>}, {<<"name">>, <<"boo">>}]}),
    save_doc(Db,"myfoo%2Fbar",{[{<<"foo2">>, <<"bar2">>}, {<<"name">>, <<"boo">>}]}),
    save_doc(Db,"_design%2FageN",{create_view("ageN", <<"javascript">>, [{<<"foo-5">>, <<"function(doc) { if (doc.age % 5 == 0 )  emit(doc.name, doc.age); }">>}])}),

    delete_db(Host, "erl-couch"),
    io:format("test complete ~n", []).



%%
%% @private
%%
%% helper function for creating view documents, borrowed from erlang_couchdb
create_view(ViewClass, Language, Views) ->
    [
        {<<"_id">>, list_to_binary("_design/" ++ ViewClass)},
        {<<"language">>, Language},
        {<<"views">>, {[
            begin
                case View of
                    {Name, Map} -> 
                        {Name, {[{<<"map">>, Map}]}};
                    {Name, Map, Reduce} ->
                        {Name, {[{<<"map">>, Map}, {<<"reduce">>, Reduce}]}}
                end
            end || View <- Views
        ]}}].

