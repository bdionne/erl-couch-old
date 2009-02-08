erl-couch : An Erlang based CouchDB client
======================

erl-couch is a CouchDB client that picks up from <a href="http://code.google.com/p/erlcouch/">erlcouch</a> (which has been discontinued). 

The idea is to complete the implementation to use ibrowse as well as inets, to see if ibrowse, with it's connection pooling is more suitable than <a href="http://github.com/ngerakines/erlang_couchdb/tree/master">erlang-couchdb</a> in terms of simulating multiple clients on the same machine. Erlang-couchdb creates a new socket for each api call and was designed to be simpler.

erl-couch spawns a process for each host and for each database. For the stress tests we're cobbling together for couchdb this hopefully will allow us to simulate multiple clients from the same machine.

src/couch.erl
-------------

This first version basically is erlcouch updated to the latest CouchDB and the with the addition of iBrowse support as well as inets. Most of the calls take an additional Options argument. For example update_doc takes a {force, true} option that retrieves the current revision and uses it to retry the update. What seems awkwatd is these Options are appended to the options passed in when creating the host and when creating the DB. When the request is finally sent, all these options are used to construct the appropriate url. This seems like a lot of list munging, particularly when each Host and each Db is a separate Pid.

src/couch2.erl
--------------

This second version removes the Options from most of the api calls, and adds an Args to the appropriate ones. Each Host and Db is a pid but the urls are constructed once and passed around through the calls. When the requests are sent over ibrowse and inets, the Args paramaters are constructed. Each api call returns either {ok, Json} or {error, Json, Raw}.

I'm not sure how useful an api at this low level is for applications, the two scenarios I have in mind are 1. testing and 2. perhaps a server side external.


