erl-couch : A couchdb client that works with inets or ibrowse
======================

erl-couch is a CouchDB client that picks up from <a href="http://code.google.com/p/erlcouch/">erlcouch</a> (which has been discontinued). 

The idea is to complete the implementation to use ibrowse as well as inets, to see if ibrowse, with it's connection pooling is more suitable than <a href="http://github.com/ngerakines/erlang_couchdb/tree/master">erlang-couchdb</a> in terms of simulating multiple clients on the same machine. Erlang-couchdb creates a new socket for each api call and was designed to be simpler.

erl-couch spans a process for each host and for each database. For the stress tests we're cobbling together for couchdb this hopefully will allow us to simulating more clients from the same machine.