package soundwave.common

import scala.collection.mutable.{AbstractMap,HashMap}

class KeyError[K](key: K) extends Throwable {
  override def toString = s"KeyError: $key"
}

/**
 * An associative container with a defined ordering. The ordering is not defined by the keys, but
 * rather by the order in which items are inserted (and possibly rearranged).
 */
abstract class OrderedMap[K, V] extends AbstractMap[K, V] {

  /**
   * Remove the element specified by key from the ordering. Does not remove key from the container
   * itself, or affect the size of the container. The container must contain key.
   */
  protected def spliceOut(key: K): Unit


  /**
   * Insert the element specified by source into the ordering in the position of the element
   * specified by dest. The container must contain source and dest.
   */
  protected def spliceIn(source: K, dest: K): Unit

  /**
   * Insert the key-value mapping at the front of the order. If the given key already exists in the
   * container, the value there is overwritten and moved to the front.
   */
  def prepend(kv: (K, V)): OrderedMap.this.type = {
    this += kv
    moveToFront(kv._1)
    return this
  }

  /**
   * Get the first key ocurring after the given key in the order. If the given key is the last key,
   * returns None. If the given key does not exist in the container, results in an exception.
   */
  def after(key: K): Option[K]

  /**
   * Move the element specified by the key source to the position of that specified by the key dest.
   * Elements after dest are shifted down by one, so that all elements except for source maintain
   * the same relative order. If source or dest is not present in the container, this results in an
   * exception. Returns the container itself.
   */
  def moveTo(source: K, dest: K): OrderedMap.this.type = {
    if (!(this contains source)) {
      throw new KeyError(source)
    } else if (!(this contains dest)) {
      throw new KeyError(dest)
    }
    if (source != dest) {
      spliceOut(source)
      spliceIn(source, dest)
    }

    return this
  }

  /**
   * Move the element specified by key to the front of the order. If key is not present in the
   * container, this results in an exception. Returns the container itself.
   */
  def moveToFront(key: K): OrderedMap.this.type = {
    if (isEmpty) {
      throw new KeyError("front")
    } else {
      moveTo(key, iterator.next._1)
    }

    return this
  }
}

class HashList[K, V] extends OrderedMap[K, V] {

  class HashListEntry(var key: K, var value: V,
                      var previous: Option[HashListEntry] = None,
                      var next: Option[HashListEntry] = None)
  {
    override def toString = s"$key -> $value"
  }

  class Iterator(private var nextEntry: Option[HashListEntry])
    extends scala.collection.Iterator[(K, V)]
  {
    def hasNext = nextEntry.isDefined
    def next: (K, V) = {
      val current = nextEntry.get
      nextEntry = current.next
      return (current.key, current.value)
    }
  }

  private val innerMap = new HashMap[K, HashListEntry]()

  private var front: Option[HashListEntry] = None
  private var back: Option[HashListEntry] = None

  override def headOption = front match {
    case Some(entry)  => Some((entry.key, entry.value))
    case None         => None
  }

  override def lastOption = back match {
    case Some(entry)  => Some((entry.key, entry.value))
    case None         => None
  }

  override def isEmpty = innerMap.isEmpty

  override def size = innerMap.size

  def +=(kv: (K, V)): this.type = {
    val (k, v) = kv

    if (innerMap contains k) {
      innerMap(k).value = v
    } else {
      // Insert after the last element
      val entry = new HashListEntry(k, v, previous = back, next = None)
      innerMap += (k -> entry)
      if (back.isDefined) {
        require(front.isDefined) // Consistency check: only a nonempty hashlist has a value for back
        back.get.next = Some(entry)
        back = Some(entry)
      } else {
        require(!front.isDefined) // Consistency check: only an empty hashlist has no back
        front = Some(entry)
        back = front
      }
    }

    return this
  }

  def -=(key: K): this.type = {
    if (innerMap contains key) {
      spliceOut(key)
      innerMap -= key
    }

    return this
  }

  def get(key: K) = innerMap get key match {
    case Some(entry)  => Some(entry.value)
    case None         => None
  }

  def iterator = new Iterator(front)

  def after(key: K) =
    if (this contains key)
      innerMap(key).next.map(_.key)
    else
      throw new KeyError(key)

  protected def spliceOut(key: K) = {
    val entry = innerMap(key)
    if (entry.previous.isDefined) {
      entry.previous.get.next = entry.next
    } else {
      require(front == Some(entry)) // Consistency check: a node with no previous node must be front
      front = entry.next
    }
    if (entry.next.isDefined) {
      entry.next.get.previous = entry.previous
    } else {
      require(back == Some(entry)) // Consistency check: a node with no next node must be back
      back = entry.previous
    }
  }

  protected def spliceIn(source: K, dest: K) = {
    val sourceEntry = innerMap(source)
    val next = innerMap(dest)
    val previous = next.previous

    // Point the previous node and the new node at each other
    sourceEntry.previous = previous
    if (previous.isDefined) {
      previous.get.next = Some(sourceEntry)
    } else {
      require(front == Some(next))
      front = Some(sourceEntry)
    }

    // Point the existing node and the new node at each other
    sourceEntry.next = Some(next)
    next.previous = Some(sourceEntry)
  }

}

object HashList {
  // Constructors

  def apply[K, V](): HashList[K, V] = new HashList[K, V]()

  def apply[K, V](kv: (K, V), kvs: (K, V)*): HashList[K, V] = HashList[K, V](kv +: kvs)

  def apply[K, V](kvs: Seq[(K, V)]): HashList[K, V] = {
    val l = HashList[K, V]()
    for (kv <- kvs) {
      l += kv
    }
    l
  }
}
