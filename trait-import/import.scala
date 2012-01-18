object TO {

	trait T {
	
		case class Wrapper(n: Int)	

		implicit def wrapInt(n: Int) = new {
			def wrap = Wrapper(n)
		}
	
	}

	object T extends T
	

}

object AO {

	import TO.T
	
	trait A {
		// def foo(w: Wrapper) doesn't work because of the dependent type
		def foo(w: T#Wrapper) = w.n
	}
	
	object A extends A

}

object all
	extends TO.T
	with AO.A

object Main extends App {

	import all._
	
	println(foo(3.wrap))

}

