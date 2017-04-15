#!/usr/bin/env bats

@test "returns an integer" {
        status=$(schminke bats/int.sch | lli)
        [[ "$status" -eq 0 ]]
}
