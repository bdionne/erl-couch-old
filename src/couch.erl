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

-module(couch).
-compile(debug_info).
-define(JSON_ENCODE(V), mochijson2:encode(V)).
-define(JSON_DECODE(V), mochijson2:decode(V)).

-ifdef(debug).
-define(DEBUG(Format, Args), io:format(Format, [Args])).
-else.
-define(DEBUG(Format, Args), true).
-endif.




-export([get_host/1]).

-export([
         create_db/2   , create_db/3,
         get_db/2      , get_db/3,
         delete_db/2   , delete_db/3
        ]).


-export([
         get_doc/2     , get_doc/3     ,
         get_doc_rev/3 , get_doc_rev/4 ,
         save_doc/2    , save_doc/3    , save_doc/4,
         update_doc/4  , update_doc/5  ,
         delete_doc/2  , delete_doc/3
        ]).
-export([
         all_dbs/1     , all_dbs/2     ,
         all_docs/1    , all_docs/2
        ]).


%% This is the record used by json.erl to encode JSON objects
%% Note that data is acually a PropList

-record(json_object, {data=[]}).


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
	RequestFunction = case proplists:get_value(ibrowse, Options) of
		true ->
				  
			fun(Method, Data, Opts) ->
				ibrowse_request(Method, Data, Opts)
			end;
		_ ->
			fun(Method, Data, Opts) ->
				inets_request(Method, Data, Opts)
			end
	end,
	NewOpts = Options ++ [{request_func, RequestFunction}],

	Pid = spawn(fun() -> host_loop(NewOpts) end),
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
	all_dbs(Host, []).

all_dbs(Host, Options) ->
	Host ! {self(), {all_dbs, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec get_db(Host::pid(), DBName::string()) -> DB::pid() | {error, json_object(), raw_json()}
%%
%% @doc Retrieves specified db from the host

get_db(Host, DBName) ->
	get_db(Host, DBName, []).

get_db(Host, DBName, Options) ->
	Host ! {self(), {get_db, DBName, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec create_db(Host::pid(), DBName::string()) -> DB::pid() | {error, json_object(), raw_json()}
%%
%% @doc Creates specified db on the host

create_db(Host, DBName) ->
	create_db(Host, DBName, []).

create_db(Host, DBName, Options) ->
	Host ! {self(), {create_db, DBName, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec delete_db(Host::pid(), DBName::string()) -> DB::pid() | {error, json_object(), raw_json()}
%%
%% @doc Deletes specified db on the host

delete_db(Host, DBName) ->
	delete_db(Host, DBName, []).

delete_db(Host, DBName, Options) ->
	Host ! {self(), {delete_db, DBName, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec all_docs(DB::pid()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc Retrieves info on all docs available on the host
%% @equiv get_doc(DB, "_all_docs")

all_docs(DB) ->
	all_docs(DB, []).

all_docs(DB, Options) ->
	get_doc(DB, "_all_docs", Options).

%% @spec get_doc(DB::pid(), DocName::string()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc Retrieves the entire contents of the specified document

get_doc(DB, DocName) ->
	get_doc(DB, DocName, []).

get_doc(DB, DocName, Options) ->
	DB ! {self(), {get_doc, DocName, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec get_doc_rev(DB::pid(), DocName::string(), Rev::integer()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @todo NOT IMPLEMENTED YET
%%
%% @doc
%%    Retrieves the entire contents of the specified document for the specified revision
%% @end

get_doc_rev(DB, DocName, Rev) ->
	get_doc_rev(DB, DocName, Rev, []).

get_doc_rev(DB, DocName, Rev, Options) ->
	DB ! {self(), {get_doc_rev, DocName, Rev, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.


%% @spec save_doc(DB::pid(), Doc::proplist()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Saves a doc to the database. The doc is assigned a server-generated ID
%% @end
save_doc(DB, Doc) ->
	DB ! {self(), {save_doc, Doc}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec save_doc(DB::pid(), DocName::string(), Doc::proplist()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Saves a doc to the database with a user-supplied ID
%% @end
save_doc(DB, DocName, Doc) ->
	save_doc(DB, DocName, Doc, []).

save_doc(DB, DocName, Doc, Options) ->
	DB ! {self(), {save_doc, DocName, Doc, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec update_doc(DB::pid(), DocName::string(), Rev::integer(), Doc::proplist()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Save a doc when the doc's revision is known
%% @end

update_doc(DB, DocName, Rev, Doc) ->
	update_doc(DB, DocName, Rev, Doc, []).

update_doc(DB, DocName, Rev, Doc, Options) ->
	DB ! {self(), {update_doc, DocName, Rev, Doc, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.

%% @spec delete_doc(DB::pid(), DocName::string()) -> {json_object(), raw_json()} | {error, json_object(), raw_json()}
%%
%% @doc
%%    Delete a doc from the database
%% @end
delete_doc(DB, DocName) ->
	delete_doc(DB, DocName, []).

delete_doc(DB, DocName, Options) ->
	DB ! {self(), {delete_doc, DocName, Options}},
	receive
		Response ->
			Response
		after 10000 ->
			{error, timeout}
	end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  INTERNAL FUNCTIONS                                                      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

host_loop(Options) ->
	receive
		{Pid, {create_db, DBName, Opts}} ->
			Reply = couch_create_db(DBName, Options ++ Opts),
			Pid ! Reply,
			host_loop(Options);
		{Pid, {get_db, DBName, Opts}} ->
			Reply = couch_get_db(DBName, Options ++ Opts),
			Pid ! Reply,
			host_loop(Options);
		{Pid, {all_dbs, Opts}} ->
			Reply = couch_all_dbs(Options ++ Opts),
			Pid ! Reply,
			host_loop(Options);
		{Pid, {delete_db, DBName, Opts}} ->
			Reply = couch_delete_db(DBName, Options ++ Opts),
			Pid ! Reply,
			host_loop(Options)
	end.

database_loop(Options) ->
	receive
		{Pid, {get_doc, DocName, Opts}} ->
			NewOpts = Options ++ Opts,
			Reply = couch_get_doc(DocName, NewOpts),
			Pid ! Reply,
			database_loop(Options);
		{Pid, {get_doc_rev, DocName, Rev, Opts}} ->
			NewOpts = Options ++ Opts,
			Reply = couch_get_doc(DocName, Rev, NewOpts),
			Pid ! Reply,
			database_loop(Options);
		{Pid, {save_doc, Doc}} ->
			Reply = couch_save_doc(Doc, Options),
			Pid ! Reply,
			database_loop(Options);
		{Pid, {save_doc, DocName, Doc, Opts}} ->
			NewOpts = Options ++ Opts,
			Reply = couch_save_doc(DocName, Doc, NewOpts),
			Pid ! Reply,
			database_loop(Options);
		{Pid, {update_doc, DocName, Rev, Doc, Opts}} ->
			NewOpts = Options ++ Opts,
			Reply = couch_update_doc(DocName, Rev, Doc, NewOpts),
			Pid ! Reply,
			database_loop(Options);
		{Pid, {delete_doc, DocName, Opts}} ->
			NewOpts = Options ++ Opts,
			Reply = couch_delete_doc(DocName, NewOpts),
			Pid ! Reply,
			database_loop(Options)
	end.

%% Internal functions

couch_create_db(DBName, Options) ->
	RequestFunc = proplists:get_value(request_func, Options),
	NewOpts = Options ++ [{db, DBName}],
	{JSON, Raw} = RequestFunc(put, "", NewOpts),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			{error, Response, Raw};
		_ ->
			Pid = spawn(fun() -> database_loop(NewOpts) end),
			Pid
	end.

couch_delete_db(DBName, Options) ->
	RequestFunc = proplists:get_value(request_func, Options),
	NewOpts = Options ++ [{db, DBName}],
	{JSON, Raw} = RequestFunc(delete, "", NewOpts),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			{error, Response, Raw};
		_ ->
			{ok, Response, Raw}
	end.

couch_all_dbs(Options) ->
	RequestFunc = proplists:get_value(request_func, Options),
	NewOpts = Options ++ [{db, "_all_dbs"}],
	{Json, _} = RequestFunc(get, "", NewOpts),
        Json.

couch_get_db(DBName, Options) ->
	RequestFunc = proplists:get_value(request_func, Options),
	NewOpts = Options ++ [{db, DBName}],
	{JSON, Raw} = RequestFunc(get, "", NewOpts),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			{error, Response, Raw};
		_ ->
			Pid = spawn(fun() -> database_loop(NewOpts) end),
			Pid
	end.

couch_get_doc(DocName, Options) ->
	NewOpts = Options ++ [{docname, DocName}],
	RequestFunc = proplists:get_value(request_func, Options),
	{JSON, Raw} = RequestFunc(get, [], NewOpts),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			{error, JSON, Raw};
		_ ->
			{Response, Raw}
	end.

couch_get_doc(_DocName, _Rev, _Options) ->
	{error, not_implemented}.

couch_save_doc(Doc, Options) ->
	FullDoc = [{"value", Doc}],
	JSONDoc = iolist_to_binary(?JSON_ENCODE(FullDoc)),
	RequestFunc = proplists:get_value(request_func, Options),

        ?DEBUG("sending json : ~s ~n",[JSONDoc]),

	{JSON, Raw} = RequestFunc(post, JSONDoc, Options),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			{error, JSON, Raw};
		_ ->
			{Response, Raw}
	end.

couch_save_doc(DocName, Doc, Options) ->
	FullDoc = [{"value", Doc}],
	JSONDoc = iolist_to_binary(?JSON_ENCODE(FullDoc)),
	RequestFunc = proplists:get_value(request_func, Options),

	NewOpts = Options ++ [{docname, DocName}],

	{JSON, Raw} = RequestFunc(put, JSONDoc, NewOpts),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			case proplists:get_value(force, Options) of
				true ->
					LatestRevDoc = couch_get_doc(DocName, Options),
					case LatestRevDoc of
						{error, _, _} ->
							LatestRevDoc;
						{#json_object{data=DataList}, _} ->
							Rev = proplists:get_value("_rev", DataList),
							?DEBUG("Revision in save ~p~n", [Rev]),
							couch_update_doc(DocName, Rev, Doc, Options)
					end;
				undefined ->
					{error, Response, Raw}
			end;
		_ ->
			{Response, Raw}
	end.

couch_update_doc(DocName, Rev, Doc, Options) ->
	FullDoc = [{"_rev", Rev}, {"value", Doc}],
	JSONDoc = iolist_to_binary(?JSON_ENCODE(FullDoc)),
	?DEBUG("JSONDoc ~s~n", [JSONDoc]),
	RequestFunc = proplists:get_value(request_func, Options),

	NewOpts = Options ++ [{docname, DocName}],

	{JSON, Raw} = RequestFunc(put, JSONDoc, NewOpts),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			{error, JSON, Raw};
		_ ->
			{Response, Raw}
	end.

couch_delete_doc(DocName, Options) ->
	RequestFunc = proplists:get_value(request_func, Options),
	NewOpts = Options ++ [{docname, DocName}],

	{JSON, Raw} = RequestFunc(delete, [], NewOpts),

	{ok, #json_object{data=Data}=Response} = JSON,

	case Data of
		[{"error", _}] ->
			{error, JSON, Raw};
		_ ->
			{Response, Raw}
	end.

%% Request functions
inets_request(Method, Data, Options) ->
	Host = couch_get_url(Options),
	?DEBUG("Requesting ~s~n", [Host]),
	{ok, {_Status, _Headers, Body}} =
		if
			Method =:= post; Method =:= put ->
				http:request(
					Method,
					{Host, [{"Content-Type", "application/json"}], [], Data},
					[],
					[]
				);
			true ->
				http:request(
					Method,
					{Host, [{"Content-Type", "application/json"}]},
					[],
					[]
				)
		end,
        ?DEBUG("received ~s ~n", [Body]),

	{?JSON_DECODE(Body), Body}.

ibrowse_request(Method, Data, Options) ->
    Host = couch_get_url(Options),
    ?DEBUG("Requesting ~s ~n", [Host]),
    {ok, _Status, _Headers, Body} =
	if
	    Method =:= post; Method =:= put ->
		ibrowse:send_req(
		  Host,
		  [{"Content-Type", "application/json"}],
		  Method,
		  Data);
	    true ->
		?DEBUG("sending ... ~s ~n",[Host]),
		ibrowse:send_req(
		  Host,
		  [{"Content-Type", "application/json"}],
		  get)
	end,
    ?DEBUG("received ~s ~n", [Body]),

    {?JSON_DECODE(Body), Body}.


%% Utility functions

couch_get_url(Options) ->
	Host = case proplists:get_value(host, Options) of
		undefined ->
			"";
		H ->
			H
	end,
	Port = case proplists:get_value(port, Options) of
		undefined ->
			"";
		P ->
			P
	end,
	Database = case proplists:get_value(db, Options) of
		undefined ->
			"";
		D ->
			D
	end,
	Docname = case proplists:get_value(docname, Options) of
		undefined ->
			"";
		Doc ->
			Doc
	end,
	URL = add_url_part(Host, "http://") ++ add_url_part(Port, ":") ++
	      add_url_part(Database, "/") ++ add_url_part(Docname, "/"),
	URL.

add_url_part(Part, Prefix) when is_integer(Part) ->
	add_url_part(integer_to_list(Part), Prefix);

add_url_part(Part, Prefix) ->
	case Part of
		"" ->
			"";
		_ ->
			lists:flatten([Prefix | Part])
	end.
