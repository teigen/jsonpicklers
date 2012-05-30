package pickles

trait Reify[A]{ self =>
  def reify(any:Any):Option[A]

  def | [T >: A, B <: T](b:Reify[B]):Reify[T] = new Reify[T]{
    def reify(any: Any) = {
      val ra:Option[T] = self.reify(any)
      ra orElse b.reify(any)
    }
  }
}

object Reify extends ReifyAnyRef with ReifyAnyVal {

  def apply[A](pf:PartialFunction[Any, A]):Reify[A] = new Reify[A] {
    def reify(any: Any) = pf.lift(any)
  }

  def opt[A](pf:PartialFunction[Any, Option[A]]):Reify[A] = new Reify[A]{
    def reify(any: Any) = pf.lift(any).flatMap(identity)
  }

  implicit val NULL    = Reify{ case null => null }

  implicit def any:Reify[Any] = anyVal | NULL | anyRef[AnyRef]

  implicit def left[A : Reify] = Reify.opt{
    case Left(value) => implicitly[Reify[A]].reify(value).map(Left(_))
  }

  implicit def right[A : Reify] = Reify.opt{
    case Right(value) => implicitly[Reify[A]].reify(value).map(Right(_))
  }

  implicit def either[A : Reify, B : Reify]:Reify[Either[A, B]] = left[A] | right[B]

  implicit def option[A : Reify] = Reify.opt{
    case Some(value) => implicitly[Reify[A]].reify(value).map(Some(_))
    case None        => None
  }
}

trait ReifyAnyRef {
  implicit def anyRef[A <: AnyRef : Manifest]:Reify[A] = new Reify[A]{
    val man = implicitly[Manifest[A]]

    def reify(any: Any) = any match {
      case ref:AnyRef =>
        val result = man >:> ClassManifest.singleType(ref)
        if (result) Some(ref.asInstanceOf[A]) else None
      case _ => None
    }
  }
}

trait ReifyAnyVal {
  implicit val boolean = Reify{ case b:Boolean => b }
  implicit val byte    = Reify{ case b:Byte    => b }
  implicit val short   = Reify{ case s:Short   => s }
  implicit val int     = Reify{ case i:Int     => i }
  implicit val long    = Reify{ case l:Long    => l }
  implicit val float   = Reify{ case f:Float   => f }
  implicit val double  = Reify{ case d:Double  => d }
  implicit val char    = Reify{ case c:Char    => c }
  implicit val unit    = Reify{ case u:Unit    => u }

  implicit val anyVal:Reify[AnyVal] = boolean | byte | short | int | long | float | double | char | unit
}