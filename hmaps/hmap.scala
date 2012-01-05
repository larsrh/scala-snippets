abstract class Key {
	type Value
}

object Key {
	def apply[V]() = new Key { type Value = V }
}

class TypedMap[-KeyType <: Key] {
	private[this] val backed = new collection.mutable.HashMap[Any, Any]()

	def get[K <: KeyType](key: K): Option[K#Value] = backed.get(key).map(_.asInstanceOf[K#Value])
	def apply[K <: KeyType](key: K): K#Value = backed(key).asInstanceOf[K#Value]
	def update[K <: KeyType, V <: K#Value](key: K, value: V) { backed(key) = value }

	override def toString = backed.toString
}

object Test {
	val map = new TypedMap[Key]

	import Key._
	val ikey1 = Key[Int]()
	val ikey2 = Key[Int]()
	val skey = Key[String]()
}

