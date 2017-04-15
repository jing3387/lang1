#!/usr/bin/env bats

EXE=sch
DIR="$BATS_TEST_DIRNAME/examples"
LLI=lli-4.0

@test "returns an integer" {
        run bash -c "${EXE} "$DIR/int.sch" | ${LLI}"
        [[ "$status" -eq 1 ]]
}

@test "returns the result of the factorial function" {
        run bash -c "${EXE} "$DIR/fac.sch" | ${LLI}"
        [[ "$status" -eq 120 ]]
}
