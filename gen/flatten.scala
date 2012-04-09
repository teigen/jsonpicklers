for(i <- 2 to 22){
//	implicit def flatten2[A1, A2, R](f:(A1, A2) => R) = (p: A1 ~ A2) => p match { case a1 ~ a2 => f(a1, a2) }      
//  implicit def tilde2[A1, A2](f:((A1, A2))) = f match { case (a1, a2) => a1 ~ a2 }
  
  def range(start:String, m:String, s:String, end:String) = (1 to i).map(m+).mkString(start, s, end)
  
  val flatten = "implicit def flatten" + i + range("[", "A", ",", ",R]") + range("(f:(","A", ",",") => R) = ") + range("(p: ","A", "~", ") => p match { ") + range("case (","a", "~" ,") => ") + range("f(", "a", ",", ") }")
  val tuple   = "implicit def tilde" + i + range("[", "A", ",", "]") + range("(f:((", "A",",","))) = f match { case ") + range("(", "a", ",", ") => ") + range("", "a", "~", "}")
  
  println(flatten)
  println(tuple)
}