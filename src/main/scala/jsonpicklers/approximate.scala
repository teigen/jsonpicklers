package jsonpicklers

import org.json4s.JsonAST.{JNothing, JField, JObject}

object Approximate {

  case class Field(name:String, approximate:Approximate){
    def apply(location:Location) = location.json match {
      case JObject(fields) =>
        val distances = fields.flatMap{ case JField(fname, _) =>
          approximate.distance(fname, name).map( _ -> fname)
        }
        distances.sortWith(_._1 < _._1) match {
          case (distance, field) :: tail if tail.forall(_._1 > distance) =>
            location(field)
          case _ => FieldLocation(JNothing, name, location)
        }
      case _ => FieldLocation(JNothing, name, location)
    }
  }

  object Exact extends Approximate {
    def distance(goal: String, input: String): Option[Int] =
      if(goal == input) Some(0) else None
  }

  case class IgnoreCase(score:Int) extends Approximate {
    def distance(goal: String, input: String): Option[Int] =
      if(goal.equalsIgnoreCase(input)) Some(score) else None
  }

  case class Levenshtein(limit:Int) extends Approximate {
    def distance(s1: String, s2: String): Option[Int] = {
      val l1 = s1.length
      val l2 = s2.length

      def min(a:Int, b:Int, c:Int) = math.min(a, math.min(b, c))

      val d = Array.ofDim[Int](l1 + 1, l2 + 1)

      for(i <- 0 to l1) d(i)(0) = i
      for(j <- 0 to l2) d(0)(j) = j

      for(i <- 1 to l1; j <- 1 to l2){
        val cost = if(s1(i-1) == s2(j-1)) 0 else 1
        d(i)(j) = min(
          d(i-1)(j  ) + 1,
          d(i  )(j-1) + 1,
          d(i-1)(j-1) + cost
        )
      }
      val result = d(l1)(l2)
      if(result <= limit)
        Some(result)
      else None
    }
  }
}

trait Approximate {
  def distance(goal:String, input:String):Option[Int]
  def apply(name:String) = Approximate.Field(name, this)
}