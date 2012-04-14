package jsonpicklers

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import net.liftweb.json.JsonAST._
import org.scalacheck.Arbitrary.arbitrary

class TypeTest extends PropSpec with GeneratorDrivenPropertyChecks {
  property("string") {
    forAll {
      s: String =>
        assert(string.unpickle(JString(s)) === Success(s, Root, JString(s)))
        assert(string.pickle(s) === JString(s))
    }
  }

  property("boolean") {
    forAll {
      b: Boolean =>
        assert(boolean.unpickle(JBool(b)) === Success(b, Root, JBool(b)))
        assert(boolean.pickle(b) === JBool(b))
    }
  }

  property("double") {
    forAll {
      d: Double =>
        assert(double.unpickle(JDouble(d)) === Success(d, Root, JDouble(d)))
        assert(double.pickle(d) === JDouble(d))
    }
  }

  property("integer") {
    forAll {
      i: Int =>
        assert(integer.unpickle(JInt(i)) === Success(i, Root, JInt(i)))
        assert(integer.pickle(i) === JInt(i))
    }
  }

  property("null") {
    assert(NULL.unpickle(JNull) === Success(null, Root, JNull))
    assert(NULL.pickle(null) === JNull)
  }

  property("bigint") {
    forAll {
      b: BigInt =>
        assert(bigint.unpickle(JInt(b)) === Success(b, Root, JInt(b)))
        assert(bigint.pickle(b) == JInt(b))
    }
  }

  property("property") {
    forAll(arbitrary[String].suchThat(!_.isEmpty), arbitrary[Int]) {
      (name, value) =>
        val json = JObject(List(JField(name, JInt(value))))
        val field = name :: integer
        assert(field.unpickle(json) === Success(value, Root, json))
        assert(field.pickle(value) === json)
    }
  }

  property("~") {

    val names = for {
      a <- arbitrary[String]
      b <- arbitrary[String]
      if !a.isEmpty && !b.isEmpty && a != b
    } yield (a, b)

    forAll(names, arbitrary[Int], arbitrary[Int]) {
      (names, aVal, bVal) =>
        val (aName, bName) = names
        val json = JObject(List(JField(aName, JInt(aVal)), JField(bName, JInt(bVal))))
        val seq = (aName :: integer) ~ (bName :: integer)
        val vals = new ~(aVal, bVal)
        assert(seq.unpickle(json) === Success(vals, Root, json))
        assert(seq.pickle(vals) === json)
    }
  }

  property("option") {
    forAll(arbitrary[String].suchThat(!_.isEmpty), arbitrary[Option[Int]]) {
      (name, value) =>
        val json = JObject(value.map(value => JField(name, JInt(value))).toList)
        val field = option(name :: integer)
        assert(field.unpickle(json) === Success(value, Root, json))
        assert(field.pickle(value) === json)
    }
  }

  property("array") {
    forAll {
      list: List[Int] =>
        val json = JArray(list.map(JInt(_)))

        assert(array(integer).unpickle(json) === Success(list, Root, json))
        assert(array(integer).pickle(list) === json)
    }
  }
}
