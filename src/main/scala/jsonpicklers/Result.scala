package jsonpicklers

import net.liftweb.json.JsonAST.JValue

sealed trait Result[+A]{
  def map[B](f:A => B):Result[B]
  def flatMap[B](f:A => Result[B]):Result[B]
  def orElse[B >: A](other: => Result[B]):Result[B]
  def filter(f:A => Boolean, message: => String):Result[A]
  
  def isSuccess:Boolean
  def isFailure = !isSuccess
}
case class Success[+A](value:A, location:Location, json:JValue) extends Result[A]{
  def map[B](f: (A) => B) = Success(f(value), location, json)
  def flatMap[B](f: (A) => Result[B]) = f(value)
  def orElse[B >: A](other: => Result[B]):Result[B] = this
  def filter(f: (A) => Boolean, message: => String) =
    if(f(value)) this else Failure(message, location, json)

  def isSuccess = true
}
case class Failure(message:String, location:Location, value:JValue) extends Result[Nothing]{
  def map[B](f: (Nothing) => B) = this
  def flatMap[B](f: (Nothing) => Result[B]) = this
  def orElse[B >: Nothing](other: => Result[B]):Result[B] = other
  def filter(f: (Nothing) => Boolean, message: => String) = this

  def isSuccess = false
}
