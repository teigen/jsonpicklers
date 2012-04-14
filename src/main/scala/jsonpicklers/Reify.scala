package jsonpicklers

import annotation.implicitNotFound
import net.liftweb.json.JsonAST.{JValue, JBool}

@implicitNotFound("Don't know how to reify ${A}")
trait Reify[A]{
  def reify:PartialFunction[Any, A]
}

object Reify {
  def apply[A](r:PartialFunction[Any, A]):Reify[A] = new Reify[A]{
    def reify = r
  }
  
  def instanceOf[A : Manifest] = Reify[A]{
    case a if manifest[A].erasure.isInstance(a) => a.asInstanceOf[A]
  }
  
  implicit val jvalue = Reify.instanceOf[JValue]

  // anyvals
  implicit val boolean = Reify[Boolean]{ case b:Boolean => b }  
  implicit val byte    = Reify[Byte]{    case b:Byte => b }  
  implicit val char    = Reify[Char]{    case c:Char => c }
  implicit val double  = Reify[Double]{  case d:Double => d }  
  implicit val float   = Reify[Float]{   case f:Float => f }
  implicit val long    = Reify[Long]{    case l:Long => l }
  implicit val int     = Reify[Int]{     case i:Int => i }  
  implicit val short   = Reify[Short]{   case s:Short => s }  
  implicit val unit    = Reify[Unit]{    case u:Unit => u }
  
  // big
  implicit val bigint     = Reify.instanceOf[BigInt]  
  implicit val bigdecimal = Reify.instanceOf[BigDecimal]
  
  // common
  implicit val string = Reify.instanceOf[String]

  implicit val NULL = Reify[Null]{
    case null => null
  }
  
  implicit val date = Reify.instanceOf[java.util.Date]

  // boxed
  implicit val boxedBoolean = Reify.instanceOf[java.lang.Boolean]
  implicit val boxedByte = Reify.instanceOf[java.lang.Byte]
  implicit val boxedCharacter = Reify.instanceOf[java.lang.Character]
  implicit val boxedDouble = Reify.instanceOf[java.lang.Double]
  implicit val boxedFloat = Reify.instanceOf[java.lang.Float]
  implicit val boxedLong = Reify.instanceOf[java.lang.Long]
  implicit val boxedInteger = Reify.instanceOf[java.lang.Integer]
  implicit val boxedShort = Reify.instanceOf[java.lang.Short]
  
  // parameterized
  implicit def list[A : Reify]:Reify[List[A]] = {
    val reify = implicitly[Reify[A]].reify
    Reify[List[A]]{
      case list:List[_] if list.forall(reify.isDefinedAt) => list.map(reify)
    }
  }

  implicit def option[A : Reify]:Reify[Option[A]] = {
    val reify = implicitly[Reify[A]].reify
    Reify[Option[A]]{
      case option:Option[_] if option.forall(reify.isDefinedAt) => option.map(reify)
    }
  }

  implicit def map[A : Reify, B : Reify] = {
    val reifyA = implicitly[Reify[A]].reify
    val reifyB = implicitly[Reify[B]].reify
    Reify[Map[A, B]]{
      case map:Map[_, _] if map.keys.forall(reifyA.isDefinedAt) && map.values.forall(reifyB.isDefinedAt) => map.map{ case (k, v) => (reifyA(k), reifyB(v)) }
    }
  }

  implicit def tilde[A : Reify, B:Reify] = {
    val ra = implicitly[Reify[A]].reify
    val rb = implicitly[Reify[B]].reify
    Reify[A ~ B]{
      case a ~ b if ra.isDefinedAt(a) && rb.isDefinedAt(b) => new ~(ra(a), rb(b))
    }
  }
}
