# Thesis
[bachelor thesis](bachelor_thesis.pdf)

# Description
The thesis presents various static optimizations on WebAssembly code, given the analysis results of sturdy-wasm (https://gitlab.rlp.net/plmz/sturdy.scala.git).
The optimizations were split into three parts: Dead code removal, constants optimizations and drops removal.
All optimizations were implemented in Scala and their correctness tested with manually created unit tests as well as
validation tests. The optimizations were then applied in a specified order on a large selection of WebAssembly files to test their effectiveness.

# Building
Use sbt to build. This will pull sturdy (The static analysis framework) and swam (a wasm parser for scala).

# Testing
First run /test/scala/fetch_files.sh to fetch sample wasm files (not required for unit tests). Then run the appropriate tests.

More information about the test results can be found in the thesis.
