import Reals

data AssertEqual = Equals

twelve = Positive $ 1 :-: 2 :-: Point (:.)
twoAndaHalf = Positive $ 2 :-: Point (5 :.: (:.))
negativePointTwo = Negative $ Point (2 :.: (:.))
zeroPointOneO = Positive $ 0 :-: Point (1 :.: 0 :.: (:.))
oneHundreth = Positive $ 0 :-: Point (0 :.: 1 :.: (:.))
pointTwo = Positive $ Point (2 :.: (:.))

assert x Equals repr
    | show x == repr = putStr ""
    | otherwise = error $ "\n\n" ++ "The following is wrong: " ++ show x ++ "\tIt should look like: " ++ repr ++ "\n\n"

test_01 = do 
    print twelve
    print twoAndaHalf
    print negativePointTwo
    print zeroPointOneO
    print oneHundreth
    print pointTwo

test_02 = do
    assert twelve               Equals "12"
    assert twoAndaHalf          Equals "2.5"
    assert negativePointTwo     Equals "-.2"
    assert zeroPointOneO        Equals "0.10"
    assert oneHundreth          Equals "0.01"
    assert pointTwo             Equals ".2"

main = do
    test_01
    test_02