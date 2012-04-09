package jsonpicklers

import net.liftweb.json.JsonAST._

trait Primitives {
  def primitive[A](name:String, p:A => JValue)(u:PartialFunction[(Location, JValue), Result[A]]) =
    JsonType[A](p, (location, json) => u.lift((location, json)).getOrElse(Failure("expected " + name, location, json)))

  val string = primitive[String]("string", JString(_)){
    case (location, json@JString(s)) => Success(s, location, json)
  }

  val boolean = primitive[Boolean]("boolean", JBool(_)){
    case (location, json@JBool(b)) => Success(b, location, json)
  }

  val double = primitive[Double]("double", JDouble(_)){
    case (location, json@JDouble(d)) => Success(d, location, json)
  }

  val integer = primitive[Int]("int", i => JInt(BigInt(i))){
    case (location, json@JInt(big)) if big.isValidInt => Success(big.intValue(), location, json)
  }

  val long = primitive[Long]("long", l => JInt(BigInt(l))){
    case (location, json@JInt(big)) if big <= BigInt(Long.MaxValue) && big >= BigInt(Long.MinValue) => Success(big.longValue(), location, json)
  }

  val bigint = primitive[BigInt]("bigint", JInt(_)){
    case (location, json@JInt(i)) => Success(i, location, json)
  }

  val NULL = primitive[Null]("null", _ => JNull){
    case (location, json@JNull) => Success(null, location, json)
  }

  def array[A](a:JsonType[A]) = primitive[List[A]]("array", value => JArray(value map a.pickle)){
    case (location, json@JArray(arr)) => arr.zipWithIndex.map{ case (j, i) => a.unpickle(location(i), j)}.foldRight[Result[List[A]]](Success(Nil, location, json)){
      (e, acc) => for {
        elem     <- e
        elements <- acc
      } yield elem :: elements
    }
  }
}
