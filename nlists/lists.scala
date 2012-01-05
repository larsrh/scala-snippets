object NLists {

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

	object NList {
		implicit def pimpList[A](list: List[A]) = new {
			def toNList[N <: Nat](implicit number: N): NList[A, N] = {
				require(list.length == number.toInt)
				val res = list.foldLeft(NNil: NList[A, Zero]) { (nlist, elem) => (elem :: nlist).asInstanceOf[NList[A, Zero]] }
				res.asInstanceOf[NList[A, N]]
			}
		}
	}

	sealed trait NList[+A, N <: Nat] {
		def ::[B >: A](elem: B): NList[B, Succ[N]] = NCons(elem, this)

		def zip[B](other: NList[B, N]): NList[(A, B), N]
		def apply[M <: Nat](implicit ev: AccessN[N, M]): A = ev(this)
	}

	case object NNil extends NList[Nothing, Zero] {
		def zip[B](other: NList[B, Zero]) = NNil
	}

	case class NCons[+A, N <: Nat](val head: A, val tail: NList[A, N]) extends NList[A, Succ[N]] {
		def zip[B](other: NList[B, Succ[N]]) = other match {
			case NCons(h, t) => (head, h) :: (tail zip t)
		}
	}
	
	sealed trait AccessN[N <: Nat, M <: Nat] {
		def apply[A](nlist: NList[A, N]): A
	}

	implicit def headAccess[N <: Nat]: AccessN[Succ[N], Zero] = new AccessN[Succ[N], Zero] {
		def apply[A](nlist: NList[A, Succ[N]]) = nlist match {
			case NCons(head, _) => head
		}
	}

	implicit def tailAccess[N <: Nat, M <: Nat](implicit ev: AccessN[N, M]): AccessN[Succ[N], Succ[M]] = new AccessN[Succ[N], Succ[M]] {
		def apply[A](nlist: NList[A, Succ[N]]) = nlist match {
			case NCons(_, tail) => ev(tail)
		}
	}

	import NList._

	val nl1: NList[Int, _2] = List(1, 2).toNList[_2]
	val nl2: NList[String, _2] = "a" :: "b" :: NNil

	val zipped: NList[(Int, String), _2] = nl1 zip nl2

	println(zipped)

	// doesn't compile
	//val zipped2 = (3 :: nl1) zip nl2

	println(zipped.apply[_1])

	// doesn't compile
	//println(zipped.apply[_2])

}
