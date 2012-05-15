package pickles

import org.scalatest.PropSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import net.liftweb.json.JsonAST._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Arbitrary

class TypeTest extends PropSpec with GeneratorDrivenPropertyChecks {
  
  val nonEmptyString = arbitrary[String].suchThat(!_.isEmpty)
  
  property("string") {
    forAll {
      s: String =>
        assert(string.unpickle(JString(s)) === Success(s, Root(JString(s))))
        assert(string.pickle(s) === JString(s))
    }
  }

  property("boolean") {
    forAll {
      b: Boolean =>
        assert(boolean.unpickle(JBool(b)) === Success(b, Root(JBool(b))))
        assert(boolean.pickle(b) === JBool(b))
    }
  }

  property("double") {
    forAll {
      d: Double =>
        assert(double.unpickle(JDouble(d)) === Success(d, Root(JDouble(d))))
        assert(double.pickle(d) === JDouble(d))
    }
  }

  property("int") {
    forAll {
      i: Int =>
        assert(int.unpickle(JInt(i)) === Success(i, Root(JInt(i))))
        assert(int.pickle(i) === JInt(i))
    }
  }

  property("null") {
    assert(NULL.unpickle(JNull) === Success(null, Root(JNull)))
    assert(NULL.pickle(null) === JNull)
  }

  property("bigint") {
    forAll {
      b: BigInt =>
        assert(bigint.unpickle(JInt(b)) === Success(b, Root(JInt(b))))
        assert(bigint.pickle(b) == JInt(b))
    }
  }

  property("property") {
    forAll(nonEmptyString, arbitrary[Int]) {
      (name, value) =>
        val json = JObject(List(JField(name, JInt(value))))
        val field = name :: int
        assert(field.unpickle(json) === Success(value, Root(json)))
        assert(field.pickle(value) === json)
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
        val json = JObject(List(JField(aName, JInt(aVal)), JField(bName, JInt(bVal))))
        val seq = (aName :: int) ~ (bName :: int)
        val vals = new ~(aVal, bVal)
        assert(seq.unpickle(json) === Success(vals, Root(json)))
        assert(seq.pickle(vals) === json)
    }
  }

  property("option (property)") {
    forAll(nonEmptyString, arbitrary[Option[Int]]) {
      (name, value) =>
        val json = JObject(value.map(value => JField(name, JInt(value))).toList)
        val field = option(name :: int)
        assert(field.unpickle(json) === Success(value, Root(json)))
        assert(field.pickle(value) === json)
    }
  }
  
  property("option (value)"){
    forAll{ opt:Option[Int] =>
      val value = option(int)
      val pickled = value.pickle(opt)
      
      opt match {
        case Some(i) => assert(pickled === JInt(i))
        case None => assert(pickled === JNull)
      }
      
      assert(value.unpickle(pickled) === Success(opt, Root(pickled)))
    }
  }

  property("array") {
    forAll {
      list: List[Int] =>
        val json = JArray(list.map(JInt(_)))

        assert(array(int).unpickle(json) === Success(list, Root(json)))
        assert(array(int).pickle(list) === json)
    }
  }
  
  property("wrap (value)"){

    case class Value(s:String)
    val value = string.wrap(Value)(Value.unapply(_).get)
    
    forAll{
      s:String =>                
        val pickled = value.pickle(Value(s))
        assert(pickled === JString(s))
        assert(value.unpickle(pickled) === Success(Value(s), Root(pickled)))
    }
  }
  
  property("wrap (object)"){
    
    case class Value(a:Int, b:String, c:Sub)
    case class Sub(d:Boolean, e:Double)
    
    val value = wrap(Value)(Value.unapply(_).get){
      ("a" :: int) ~
      ("b" :: string) ~
      ("c" :: wrap(Sub)(Sub.unapply(_).get){
        ("d" :: boolean) ~
        ("e" :: double)
      })
    } 
    
    forAll{ (a:Int, b:String, d:Boolean, e:Double) =>
      val v = Value(a, b, Sub(d, e))      
      val pickled = value.pickle(v)
      assert(pickled === JObject(List(
        JField("a", JInt(a)),
        JField("b", JString(b)),
        JField("c", JObject(List(
          JField("d", JBool(d)),
          JField("e", JDouble(e))))))))
      
      assert(value.unpickle(pickled) === Success(v, Root(pickled)))
    }
  }
  
  property("or"){
    val or = either(string, int)
    
    forAll{ e:Either[String, Int] =>      
      val pickled = or.pickle(e)
      e match {
        case Left(value) => assert(pickled === JString(value))
        case Right(value) => assert(pickled === JInt(value))
      }
      
      assert(or.unpickle(pickled) === Success(e, Root(pickled)))
    }
  }
  
  property("unique"){
    val values = unique(array(int))
    val regular = array(int)
    
    forAll{ set:Set[Int] =>
      val pickled = values.pickle(set.toList)
      assert(pickled === regular.pickle(set.toList))
      assert(values.unpickle(pickled) === regular.unpickle(pickled))
    }
    
    forAll(arbitrary[List[Int]].suchThat(l => l.distinct.size < l.size)){ 
      list =>
        intercept[RuntimeException]{
          values.pickle(list)
        }
        
        assert(values.unpickle(JArray(list.map(JInt(_)))).isFailure === true)
    }
  }
}
