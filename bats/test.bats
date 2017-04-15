#!/usr/bin/env bats

@test "returns an integer" {
        status=$(schminke "$BATS_TEST_DIRNAME/int.sch" | lli)
        [[ "$status" -eq 0 ]]
}

@test "returns the result of the factorial function" {
        status=$(schminke "$BATS_TEST_DIRNAME/fac.sch" | lli)
        [[ "$status" -eq 120 ]]
}
