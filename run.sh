#!/bin/sh
gitroot=$(git rev-parse --show-toplevel)
scala -classpath $gitroot/build/lib/project.jar:$gitroot/lib/antlr.jar compile.Compiler "$@"
