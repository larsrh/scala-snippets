// Exploration of <http://apocalisp.wordpress.com/2011/07/01/monads-are-dominoes/>

trait Category {
	type L
	type H >: L
	type ->>[_ >: L <: H, _ >: L <: H]

	def id[A >: L <: H]: A ->> A
	def compose[A >: L <: H, B >: L <: H, C >: L <: H](f: A ->> B, g: B ->> C): A ->> C
}

object Dominoes {

	sealed trait Number {
		val n: Int
	}

	sealed trait Zero extends Number {
		val n: Int = 0
	}

	sealed trait Succ[N <: Number] extends Number

	implicit val zero: Zero = new Zero {}
	implicit def succ[N <: Number](implicit prev: N): Succ[N] = new Succ[N] { val n = prev.n + 1 }

	type _0 = Zero
	type _1 = Succ[_0]
	type _2 = Succ[_1]
	type _3 = Succ[_2]

	case class Domino[N1 <: Number, N2 <: Number]() {
		def fst(implicit value: N1) = value.n
		def snd(implicit value: N2) = value.n

		def show(implicit v1: N1, v2: N2) = "[" + fst + "|" + snd + "]"
	}

	object DominoCategory extends Category {
		type L = Nothing
		type H = Number
		type ->>[N1 <: Number, N2 <: Number] = Domino[N1, N2]
		
		def id[N <: Number]: Domino[N, N] = Domino()
		def compose[A <: Number, B <: Number, C <: Number](d1: Domino[A, B], d2: Domino[B, C]): Domino[A, C] = Domino()
	}

}

object Test extends App {

	import Dominoes._

	val cat = DominoCategory

	val d11 = cat.id[_1]
	println(d11.show)

	val d01 = Domino[_0, _1]()
	val d12 = Domino[_1, _2]()

	val d02: Domino[_0, _2] = cat.compose(d01, d12)
	println(d02.show)

}

