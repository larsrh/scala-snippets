object ImplicitMapping {

	implicit def map[A, B](implicit f: A => B): List[A] => List[B] = _ map f

	case class Bipp(i: Int)
	case class Bopp(i: Int)

	implicit def bipp2Bopp(b: Bipp) = Bopp(b.i)

	implicitly[Bipp => Bopp]
	implicitly[List[Bipp] => List[Bopp]]
	implicitly[List[List[Bipp]] => List[List[Bopp]]]

}

