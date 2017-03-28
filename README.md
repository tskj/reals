*Lazy implementation of the real numbers in Haskell*

One of the interesting (and unique) features about Haskell is its lazy
evaluation.
By implementing the real numbers lazily, it should be possible to get infinite
precision arithmetic (though at the cost of memory and efficiency).

A lot of algorithms, for example using Newton's method for finding roots to
calculate the square root of two (an irrational number), one can define it in
such a way as to terminate when a given precision is found.

In this way, you can ask for the square root of two with 10 ^ -5 precision,
that is five decimal places, and the algorithm will calculate it for you.

It is very interesting to be able to decouple the logic of the arithmetic
from the algorithm itself in this way, and let the language handle the
heavy lifting for us.
