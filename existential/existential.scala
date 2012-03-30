// Solution to <http://pastebin.com/pc764btn>

object Exercises {

	trait Forall[F[_]] {
		def apply[A]: F[A]
	}
	 
	trait Exists[F[_]] {
		type A
		def apply: F[A]
	}
	 
	type Not[A] = A => Nothing
	 
	def notNotExists[F[_]](e: Exists[F]): Not[Forall[({type f[x] = Not[F[x]]})#f]] = (a: Forall[({type f[x] = Not[F[x]]})#f]) =>
		uncurry[F, Nothing](a)(e)
	 
	def notNotForall[F[_]](a: Forall[F]): Not[Exists[({type f[x] = Not[F[x]]})#f]] = (e: Exists[({type f[x] = Not[F[x]]})#f]) =>
		e.apply(a.apply[e.A])
	 
	def curry[F[_], A](f: Exists[F] => A): Forall[({ type f[x] = F[x] => A })#f] = new Forall[({ type f[x] = F[x] => A })#f] {
		def apply[B]: F[B] => A = (fa: F[B]) => {
			f(new Exists[F] {
				type A = B
				def apply = fa
			})
		}
	}
	 
	def uncurry[F[_], A](f: Forall[({ type f[x] = F[x] => A })#f]): Exists[F] => A = (ex: Exists[F]) =>
		f.apply[ex.A](ex.apply)

}
