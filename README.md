erl-couch : An Erlang based CouchDB client
======================

2/22/09
========
erl-couch is a CouchDB client that is now written as a gen_server (suggestion from Paul Davis). It starts and stops either ibrowse or inets, as needed, so it run pretty much by itself. It requires the couchdb bundled mochijson2 and ibrowse.

It's started by calling:

    erl_couch:start(Options)

where Option is a list, .eg.

    erl_couch:start([{host, "127.0.0.1"}, {port, 5984},{ibrowse, true}]).

calling erl_couch:stop() takes care of stopping ibrowse


2/13/09
=======

erl-couch is a CouchDB client that picks up from <a href="http://code.google.com/p/erlcouch/">erlcouch</a> (which has been discontinued). 

erl-couch spawns a process for each host and for each database. For the stress tests we're cobbling together for couchdb this hopefully will allow us to simulate multiple clients from the same machine.

src/erl-couch.erl
-------------

This new version adds support for ibrowse, as well as the default inets. Note that inets or ibrowse must be started first. One enhancement over erlcouch was to remove many of the options that were passed around with each call as they were used solely to construct urls, many of which stay the same form call to call (.eg. the host). It's also been upgraded to use mochijson2 which is embedded in CouchDB.

Each Host and Db is a separate pid, so multiple hosts and databases can be opened concurrently. The Args paramaters are constructed. Each api call returns either {ok, Json} or {error, Json, Raw}. The API is very simple, all the munging of JSON docs has to be done outside, there are no calls that take parameter and construct the JSON.

I'm not sure how useful an api at this low level is for applications, the two scenarios I have in mind are 1. testing and 2. perhaps a server side external.


