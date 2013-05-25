#!/bin/sh
rebar compile
if [[ $? == 0 ]]; then
	erl +K true -pa `pwd`/ebin `pwd`/deps/*/ebin -boot start_sasl $*
fi
