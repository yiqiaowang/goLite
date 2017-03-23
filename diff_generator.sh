#!/bin/bash

#only param is a single goLite file
go_lite_file=$1

#generated js code file name
js_file="${go_lite_file%.go}.js"

#pipe generated js code output here
go_lite_output_file="${go_lite_file%.go}.goLite.output"

#pipe output from canonical go compiler here
go_output_file="${go_lite_file%.go}.go.output"

#run goLite file on go compiler, pipe to output file
go run $go_lite_file 1&> $go_output_file

#compiler file on goLite compiler
./run.sh $go_lite_file

#run compiler js file with node, pipe to output file
node $js_file 1&> $go_lite_output_file

#diff program outputs
diff $go_lite_output_file $go_output_file
