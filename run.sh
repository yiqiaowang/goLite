#/bin/bash

args="$@"

# first arg should be filename
go_lite_file="$1"

# execute compiler
stack exec golite-exe -- $args -c

# replace goLite file with .js extension
# js_file="${go_lite_file%.go}.js"

# execute generated code with system npm
# node $js_file
