def factorial(a: Int): Int = {
	if (a == 1) 1 else a * factorial(a-1)
}

def tail_factorial(x: Int): Int = {
	def fact_iter(x: Int, count: Int, a: Int): Int = {
		if (count == x) count * a
		else fact_iter(x, count+1, a*count)
	}
	fact_iter(x, 1, 1)
}

println(tail_factorial(10))