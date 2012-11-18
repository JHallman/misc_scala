// finds the longest collatz sequence starting under given number.


// returns the size of collatz sequence for a given n
def collatz_size(count: Int, n: Int): Int = {
    if (n == 1) count
    else if (n % 2 == 0) collatz_size(count+1, n/2)
    else collatz_size(count+1, 3 * n + 1)
}

def iter(max: Int, curr: Int): Int = {
    if (curr == 0) max
    else {
        val next = collatz_size(1, curr)
        if (next > max) {
            iter(next, curr - 1)
        }
        else iter(max, curr - 1)
    }
}

println(iter(0, 1000000))
//println(collatz_size(1, 13))