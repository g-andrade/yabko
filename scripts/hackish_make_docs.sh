#!/usr/bin/env bash
set -ex

# sigh.....
rebar3 as generate_documentation compile
mkdir -p _build/generate_documentation/lib/yabko/doc/
cp -p overview.edoc _build/generate_documentation/lib/yabko/doc/
erl -pa _build/generate_documentation/lib/*/ebin -noshell -run edoc_run application "yabko"
erl -pa _build/generate_documentation/lib/*/ebin -noshell -run edoc_run application "yabko" '[{doclet, edown_doclet}, {top_level_readme, {"README.md", "https://github.com/g-andrade/yabko", "master"}}]'
rm -rf doc
mv _build/generate_documentation/lib/yabko/doc ./
sed -i -e 's/^\(---------\)$/\n\1/g' README.md
rm doc/*.{html,css,png,edoc} doc/edoc-info
