<h1>Inoteefy</h1>

Description
========

Erlang inotify wrapper

Inspired by https://github.com/massemanet/inotify
Port code completely taken from that work.

Short instructions
========

To watch some file or directory

inoteefy:watch("path/to/watch", fun(Event) -> some_sensible_work(Event) end).

To stop watching

inoteefy:unwatch("path/to/watch").

And don't forget to start application!