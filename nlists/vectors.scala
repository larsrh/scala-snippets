object NVectors {

	sealed trait Nat {
		def toInt: Int
	}

	sealed trait Zero extends Nat {
		def toInt = 0
	}

	sealed trait Succ[N <: Nat] extends Nat

	type _0 = Zero
	type _1 = Succ[_0]
	type _2 = Succ[_1]

	implicit val zero: Zero = new Zero {}
	implicit def nat[N <: Nat](implicit pred: N): Succ[N] = new Succ[N] { def toInt = pred.toInt + 1 }

	object NVector {
		implicit def pimpVector[A](vector: Vector[A]) = new {
			def toNVector[N <: Nat](implicit number: N): NVector[A, N] = {
				require(vector.length == number.toInt)
				new NVector[A, N](vector)
			}
		}

		def empty[A] = new NVector[A, Zero](Vector())
	}

	class NVector[+A, N <: Nat] private(val underlying: Vector[A]) {
		def apply[M <: Nat](implicit ev: GreaterThan[N, M], number: M): A = underlying(number.toInt)
		def +:[B >: A](elem: B): NVector[B, Succ[N]] = new NVector[B, Succ[N]](elem +: underlying)
		def zip[B](other: NVector[B, N]) = new NVector[(A, B), N](underlying zip other.underlying)
	}
	
	sealed trait GreaterThan[N <: Nat, M <: Nat]
	
	implicit def greaterThanBase[N <: Nat]: GreaterThan[Succ[N], N] = new GreaterThan[Succ[N], N] {}
	implicit def greaterThanInduct[N <: Nat, M <: Nat](implicit ev: GreaterThan[N, M]): GreaterThan[Succ[N], M] = new GreaterThan[Succ[N], M] {}

	import NVector._

	val nv1: NVector[Int, _2] = Vector(1, 2).toNVector[_2]
	val nv2: NVector[String, _2] = "a" +: "b" +: NVector.empty

	val zipped: NVector[(Int, String), _2] = nv1 zip nv2

	println(zipped)

	// doesn't compile
	//val zipped2 = (3 +: nv1) zip nv2

	println(zipped.apply[_1])

	// doesn't compile
	//println(zipped.apply[_2])

}
