object HLists {

	trait HList
	case class HCons[H, T <: HList](head: H, tail: T) extends HList {
		def ::[T](v: T) = HCons(v, this)
		def apply[N <: Nat, R](n: N)(implicit access: AccessN[HCons[H, T], N, R]) =
			access(this)
		def getType[E](implicit access: AccessType[HCons[H, T], E]) =
			access(this)
	}
	
	object HNil extends HList {
		def ::[T](v: T) = HCons(v, this)
	}

	trait Nat
	trait Zero extends Nat
	trait Succ[N <: Nat] extends Nat
	type Two = Succ[Succ[Zero]]

	trait AccessN[L <: HList, N <: Nat, R] {
		def apply(l: L): R
	}

	implicit def accessZero[H, T <: HList] =
		new AccessN[HCons[H, T], Zero, H] {
			def apply(l: HCons[H, T]) = l.head
		}

	implicit def accessN[H, T <: HList, N <: Nat, R](implicit accessTail: AccessN[T, N, R]) =
		new AccessN[HCons[H, T], Succ[N], R] {
			def apply(l: HCons[H, T]) = accessTail(l.tail)
		}

	val _0 = new Zero {}
	val _1 = new Succ[Zero] {}
	val _2 = new Succ[Succ[Zero]] {}
	val _3 = new Succ[Succ[Succ[Zero]]] {}
	
	trait AccessType[L <: HList, E] {
		def apply(l: L): E
	}
	
	implicit def accessZeroType[E, T <: HList] =
		new AccessType[HCons[E, T], E] {
			def apply(l: HCons[E, T]) = l.head
		}
	
	implicit def accessNType[E, H, T <: HList](implicit accessTail: AccessType[T, E]) =
		new AccessType[HCons[H, T], E] {
			def apply(l: HCons[H, T]) = accessTail(l.tail)
		}

	val list = 3 :: "foo" :: 5.3 :: HNil
	
	val s1: String = list(_1)
	val s2: String = list.getType[String]

}

