#!/usr/bin/env bats

EXE=schminke
LLI=lli-4.0

@test "returns an integer" {
        run bash -c "${EXE} "$BATS_TEST_DIRNAME/examples/int.sch" | ${LLI}"
        [[ "$status" -eq 1 ]]
}

@test "returns the result of the factorial function" {
        run bash -c "${EXE} "$BATS_TEST_DIRNAME/examples/fac.sch" | ${LLI}"
        [[ "$status" -eq 120 ]]
}
