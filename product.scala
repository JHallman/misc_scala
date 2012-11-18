def product(f: Int => Int): (Int, Int) => Int = {
	def productF(a: Int, b: Int): Int = {
		if (a > b) 1
		else f(a) * productF(a+1, b)
	}
	productF
}

def prod(f: Int => Int)(a: Int, b: Int): Int = {
	if (a > b) 1 else f(a) * prod(f)(a+1,b)
}

def fact(x: Int) = prod(x => x)(1, x)

println(fact(4))

// def gen(g: (Int, Int) => Int, i: Int)(f: Int => Int)(a: Int, b: Int): Int = {
// 	if (a > b) i else g(f(a), gen(g)(f)(a+1,b))
// }

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, i: Int)(a: Int, b: Int): Int = {
	if (a > b) i
	else combine(f(a), mapReduce(f, combine, i)(a+1,b))
}

def prd(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x,y) => x*y, 1)(a,b)

def factorial(x: Int)= prd(x => x)(1,x)

println(fact(5))







