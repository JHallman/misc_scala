def sqrt(x: Double): Double = {
    def abs(a: Double) = if (a < 0) -a else a
    def good_enough(guess: Double): Boolean = {
        val threshold = 0.001
        abs(guess * guess - x) / x < threshold
    }
    def improve(guess: Double): Double = {
        (guess + x / guess) / 2
    }
    def sqrt_iter(guess: Double): Double = {
        if (good_enough(guess)) guess
        else sqrt_iter(improve(guess))
    }
    sqrt_iter(1)
}

println(sqrt(2))
println(sqrt(9))

import math.abs

def fixed_point(f: Double => Double)(initial: Double): Double = {
    val threshold = 0.0000001
    def good_enough(x: Double, y: Double): Boolean = {
        abs((x-y)/x)/x < threshold
    }
    def iterate(guess: Double): Double = {
        val next = f(guess)
        if (good_enough(guess, next)) next
        else iterate(next)
    }
    iterate(initial)
}

def average_damp(f: Double => Double)(x: Double) = (x + f(x))/2

def sqrt_fixed(x: Double): Double = {
    fixed_point(average_damp(y => x/y))(1)
}

println(sqrt_fixed(2))
println(sqrt_fixed(9))

