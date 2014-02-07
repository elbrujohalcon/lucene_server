#!/bin/sh
cp `find /usr/local/lib/erlang/lib/ -name OtpErlang.jar | sort | tail -n1` priv/
