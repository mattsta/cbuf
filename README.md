cbuf: Erlang Circular Buffer/List/LIFO Queue using ETS
------------------------------------------------------

Building
========
Compile:
        rebar compile

Test:
        rebar eunit


See cbuf_test/0 in src/cbuf.erl for sanity tests.

API Usage
=========
* Create a circular list of size N (start_link)
* Add items to the list (add)
* Retrieve the most recent N items (entries)

cbuf was created to serve a bounded scrollback for a web based interactive terminal.
