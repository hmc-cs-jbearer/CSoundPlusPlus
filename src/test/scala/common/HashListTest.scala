package soundwave

import org.scalatest.FunSuite
import org.scalatest.Matchers
import scala.collection.mutable.Map

import common.HashList

class HashListSuite extends FunSuite with Matchers {

  def check[K, V](test: HashList[K, V], expected: Seq[(K, V)]) = {
    test.size should equal (expected.size)
    val testIterator = test.iterator
    val expectedIterator = expected.iterator
    while (testIterator.hasNext) {
      if (testIterator.next != expectedIterator.next) {
        fail(s"$test did not equal $expected")
      }
    }
    test.headOption should equal (expected.headOption)
    test.lastOption should equal (expected.lastOption)
  }

  def shouldThrow[T](test: =>T) =
    try {
      test
      fail("Expected exception")
    } catch {
      case _: Throwable => succeed
    }

  implicit class HashListTester[K, V](input: HashList[K, V]) {
    def ?=(expected: Seq[(K, V)]) = check(input, expected)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Constructor tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("constructor.empty") {
    HashList[String, Int]() ?= Seq()
  }

  test("constructor.oneMapping") {
    HashList("foo" -> 1) ?= Seq("foo" -> 1)
  }

  test("constructor.twoMappings") {
    HashList("foo" -> 1, "bar" -> 2) ?= Seq("foo" -> 1, "bar" -> 2)
  }

  test("constructor.variadic") {
    HashList(
      "foo" -> 1,
      "bar" -> 2,
      "baz" -> 3
    ) ?=
    Seq(
      "foo" -> 1,
      "bar" -> 2,
      "baz" -> 3
    )
  }

  test("constructor.list") {
    val l = Seq("foo" -> 1, "bar" -> 2, "baz" -> 3)
    HashList(l) ?= l
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Prepend tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("prepend.empty") {
    val l = HashList[String, Int]()
    l prepend ("foo" -> 1)
    l ?= Seq("foo" -> 1)
  }

  test("prepend.nonempty") {
    val l = HashList("foo" -> 1)
    l prepend ("bar" -> 2)
    l ?= Seq("bar" -> 2, "foo" -> 1)
  }

  test("prepend.overwrite") {
    val l = HashList("foo" -> 1)
    l prepend ("foo" -> 2)
    l ?= Seq("foo" -> 2)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Append tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("append.empty") {
    val l = HashList[String, Int]()
    l += ("foo" -> 1)
    l ?= Seq("foo" -> 1)
  }

  test("append.nonempty") {
    val l = HashList("foo" -> 1)
    l += ("bar" -> 2)
    l ?= Seq("foo" -> 1, "bar" -> 2)
  }

  test("append.overwrite") {
    val l = HashList("foo" -> 1)
    l += ("foo" -> 2)
    l ?= Seq("foo" -> 2)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Erase tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("erase.empty") {
    val l = HashList[String, Int]()
    l -= "foo"
    l ?= Seq()
    l should be ('Empty)
  }

  test("erase.notPresent") {
    val l = HashList("bar" -> 1)
    l -= "foo"
    l ?= Seq("bar" -> 1)
  }

  test("erase.middle") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l -= "bar"
    l ?= Seq("foo" -> 1, "baz" -> 3)
  }

  test("erase.front") {
    val l = HashList("foo" -> 1, "bar" -> 2)
    l -= "foo"
    l ?= Seq("bar" -> 2)
  }

  test("erase.back") {
    val l = HashList("foo" -> 1, "bar" -> 2)
    l -= "bar"
    l ?= Seq("foo" -> 1)
  }

  test("erase.only") {
    val l = HashList("foo" -> 1)
    l -= "foo"
    l ?= Seq()
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Get tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("get.empty") {
    HashList[String, Int]().get("foo") should be (None)
  }

  test("get.notPresent") {
    HashList("foo" -> 1).get("bar") should be (None)
  }

  test("get.success") {
    HashList("foo" -> 1, "bar" -> 2, "baz" -> 3).get("bar") should be (Some(2))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // After tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("after.front") {
    HashList("foo" -> 1, "bar" -> 2) after "foo" should be (Some("bar"))
  }

  test("after.middle") {
    HashList("foo" -> 1, "bar" -> 2, "baz" -> 3) after "bar" should be (Some("baz"))
  }

  test("after.back") {
     HashList("foo" -> 1, "bar" -> 2) after "bar" should be (None)
  }

  test("after.invalid.notPresent") {
    shouldThrow {
      HashList("foo" -> 1, "bar" -> 2, "baz" -> 3) after "wuzzle"
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Move to front tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("moveToFront.front") {
    val l = HashList("foo" -> 1, "bar" -> 2)
    l moveToFront "foo"
    l ?= Seq("foo" -> 1, "bar" -> 2)
  }

  test("moveToFront.middle") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l moveToFront "bar"
    l ?= Seq("bar" -> 2, "foo" -> 1, "baz" -> 3)
  }

  test("moveToFront.last") {
    val l = HashList("foo" -> 1, "bar" -> 2)
    l moveToFront "bar"
    l ?= Seq("bar" -> 2, "foo" -> 1)
  }

  test("moveToFront.invalid.empty") {
    shouldThrow {
      HashList[String, Int]().moveToFront("foo") ?= Seq()
    }
  }

  test("moveToFront.invalid.notPresent") {
    shouldThrow {
      HashList[String, Int]("foo" -> 1).moveToFront("bar") ?= Seq("foo" -> 1)
    }
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////
  // Move to * tests
  //////////////////////////////////////////////////////////////////////////////////////////////////

  test("moveTo.middle.front") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l.moveTo("bar", "foo")
    l ?= Seq("bar" -> 2, "foo" -> 1, "baz" -> 3)
  }

  test("moveTo.back.front") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l.moveTo("baz", "foo")
    l ?= Seq("baz" -> 3, "foo" -> 1, "bar" -> 2)
  }

  test("moveTo.front.middle") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l.moveTo("foo", "bar")
    l ?= Seq("foo" -> 1, "bar" -> 2, "baz" -> 3)
  }

  test("moveTo.middle.middle.forward") {
    val l = HashList("foo" -> 1, "bar1" -> 2, "bar2" -> 3, "baz" -> 4)
    l.moveTo("bar1", "bar2")
    l ?= Seq("foo" -> 1, "bar1" -> 2, "bar2" -> 3, "baz" -> 4)
  }

  test("moveTo.middle.middle.backward") {
    val l = HashList("foo" -> 1, "bar1" -> 2, "bar2" -> 3, "baz" -> 4)
    l.moveTo("bar2", "bar1")
    l ?= Seq("foo" -> 1, "bar2" -> 3, "bar1" -> 2, "baz" -> 4)
  }

  test("moveTo.back.middle") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l.moveTo("baz", "bar")
    l ?= Seq("foo" -> 1, "baz" -> 3, "bar" -> 2)
  }

  test("moveTo.front.back") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l.moveTo("foo", "baz")
    l ?= Seq("bar" -> 2, "foo" -> 1, "baz" -> 3)
  }

  test("moveTo.middle.back") {
    val l = HashList("foo" -> 1, "bar" -> 2, "baz" -> 3)
    l.moveTo("bar", "baz")
    l ?= Seq("foo" -> 1, "bar" -> 2, "baz" -> 3)
  }

  test("moveTo.invalid.noSource") {

  }

  test("moveTo.invalid.noDest") {

  }

}
