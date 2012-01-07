// from Adrian Moors
object LubFun1 {

	class F[T <: F[T]]
	class A extends F[A]
	class B extends F[B]

	// List[F[_ >: B with A	<: F[_ >: B with A <: ScalaObject]]]
	val l = List(new A, new B)

}

