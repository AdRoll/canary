#!/bin/sh

erl -pa ebin deps/*/ebin -args_file vm.args -config canary $@