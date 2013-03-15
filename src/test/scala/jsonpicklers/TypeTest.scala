package jsonpicklers

import Picklers._

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.json4s.JsonAST._
import org.scalacheck.Arbitrary.arbitrary

class TypeTest extends PropSpec with GeneratorDrivenPropertyChecks {
  
  val nonEmptyString = arbitrary[String].suchThat(!_.isEmpty)
  
  property("string") {
    forAll {
      s: String =>
        assert(string.unpickle(JString(s)).get === s)
        assert(string.pickle(s).get === JString(s))
    }
  }

  property("boolean") {
    forAll {
      b: Boolean =>
        assert(boolean.unpickle(JBool(b)).get === b)
        assert(boolean.pickle(b).get === JBool(b))
    }
  }

  property("double") {
    forAll {
      d: Double =>
        assert(double.unpickle(JDouble(d)).get === d)
        assert(double.pickle(d).get === JDouble(d))
    }
  }

  property("int") {
    forAll {
      i: Int =>
        assert(int.unpickle(JInt(i)).get === i)
        assert(int.pickle(i).get === JInt(i))
    }
  }

  property("null") {
    assert(NULL.unpickle(JNull).isSuccess)
    assert(NULL.pickle(null).get === JNull)
  }

  property("bigint") {
    forAll {
      b: BigInt =>
        assert(bigint.unpickle(JInt(b)).get === b)
        assert(bigint.pickle(b).get == JInt(b))
    }
  }

  property("property") {
    forAll(nonEmptyString, arbitrary[Int]) {
      (name, value) =>
        val json = JObject(name -> JInt(value))
        val field = name :: int
        assert(field.unpickle(json).get === value)
        assert(field.pickle(value).get === json)
    }
  }

  property("~") {

    val names = for {
      a <- nonEmptyString
      b <- nonEmptyString
      if a != b
    } yield (a, b)

    forAll(names, arbitrary[Int], arbitrary[Int]) {
      (names, aVal, bVal) =>
        val (aName, bName) = names
        val json = JObject(aName -> JInt(aVal), bName -> JInt(bVal))
        val seq = (aName :: int) ~ (bName :: int)
        val vals = new ~(aVal, bVal)
        assert(seq.unpickle(json).get === vals)
        assert(seq.pickle(vals).get === json)
    }
  }

  property("option (property)") {
    forAll(nonEmptyString, arbitrary[Option[Int]]) {
      (name, value) =>
        val json = JObject(value.map(value => name -> JInt(value)).toList)
        val field = option(name :: int)
        assert(field.unpickle(json).get === value)
        assert(field.pickle(value).get === json)
    }
  }
  
  property("option (value)"){
    forAll{ opt:Option[Int] =>
      val value = option(int)
      val Some(pickled) = value.pickle(opt)
      
      opt match {
        case Some(i) => assert(pickled === JInt(i))
        case None => assert(pickled === JNothing)
      }
      
      assert(value.unpickle(pickled).get === opt)
    }
  }

  property("array") {
    forAll {
      list: List[Int] =>
        val json = JArray(list.map(JInt(_)))

        assert(array(int).unpickle(json).get === list)
        assert(array(int).pickle(list).get === json)
    }
  }
  
  property("xmap (value)"){

    case class Value(s:String)
    val value = string.xmap(Value)(Value.unapply(_).get)
    
    forAll{
      s:String =>                
        val Some(pickled) = value.pickle(Value(s))
        assert(pickled === JString(s))
        assert(value.unpickle(pickled).get === Value(s))
    }
  }
  
  property("xmap (object)"){
    
    case class Value(a:Int, b:String, c:Sub)
    case class Sub(d:Boolean, e:Double)
    
    val value = xmap(Value)(Value.unapply(_).get){
      ("a" :: int) ~
      ("b" :: string) ~
      ("c" :: xmap(Sub)(Sub.unapply(_).get){
        ("d" :: boolean) ~
        ("e" :: double)
      })
    } 
    
    forAll{ (a:Int, b:String, d:Boolean, e:Double) =>
      val v = Value(a, b, Sub(d, e))      
      val Some(pickled) = value.pickle(v)
      assert(pickled === JObject(
        ("a", JInt(a)),
        ("b", JString(b)),
        ("c", JObject(
          ("d", JBool(d)),
          ("e", JDouble(e))))))
      
      assert(value.unpickle(pickled).get === v)
    }
  }
  
  property("or"){
    val or = either(string, int)
    
    forAll{ e:Either[String, Int] =>      
      val Some(pickled) = or.pickle(e)
      e match {
        case Left(value) => assert(pickled === JString(value))
        case Right(value) => assert(pickled === JInt(value))
      }
      
      assert(or.unpickle(pickled).get === e)
    }
  }
}
