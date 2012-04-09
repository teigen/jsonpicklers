package jsonpicklers

import annotation.implicitNotFound

@implicitNotFound("Don't know how to reify ${A}")
trait Reify[A]{
  def reify:PartialFunction[Any, A]
}

object Reify {
  def apply[A](r:PartialFunction[Any, A]):Reify[A] = new Reify[A]{
    def reify = r
  }

  implicit val string = Reify[String]{
    case s:String => s
  }

  implicit val int = Reify[Int]{
    case i:Int => i
  }
  
  implicit val jInteger = Reify[java.lang.Integer]{
    case i:java.lang.Integer => i
  }

  implicit val bigint = Reify[BigInt]{
    case b:BigInt => b
  }

  implicit val double = Reify[Double]{
    case d:Double => d
  }

  implicit val boolean = Reify[Boolean]{
    case b:Boolean => b
  }
  
  implicit val jBoolean = Reify[java.lang.Boolean]{
    case b:java.lang.Boolean => b
  }
  
  implicit val NULL = Reify[Null]{
    case null => null
  }

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
