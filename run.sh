#/bin/bash

args="$@"

# first arg should be filename
go_lite_file="$1"

# execute compiler
stack exec golite-exe -- $args

# replace goLite file with .js extension
#js_file="${go_lite_file%.go}.js"

# execute generated code with system npm
#npm $js_file
