import TestHarness

import Naturals

zero = 0::N
one = 1::N
two = 2::N
five = 5::N

test_01 = do
    assert zero Equals "0"
    assert one Equals "1"
    assert two Equals "2"
    assert five Equals "5"
    assert (zero + zero) Equals "0"
    assert (zero + one) Equals "1"
    assert (zero + two) Equals "2"
    assert (zero + five) Equals "5"
    assert (zero * zero) Equals "0"
    assert (zero * one) Equals "0"
    assert (zero * two) Equals "0"
    assert (zero * five) Equals "0"
    assert (one + two) Equals "3"
    assert (two + five) Equals "7"
    assert (one * two) Equals "2"
    assert (one * five) Equals "5"
    assert (two * five) Equals "10"
    assert (two * five) NotEquals "a"

main = do
    test_01