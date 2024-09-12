package com.bura

object TryTry extends App {
 type Error = String

  def splitToList(x: String): List[String] = x.split("\\s+").toList
  def ok(x: List[String]): Either[Error, List[Double]] = {
    val pat = "\\d+(\\.\\d*)??".r

    x match {
      case x if x.forall(item => pat.matches(item)) => Right(x.map(_.toDouble))
      case _ => Left("Not Digit found")
    }
  }



  println(ok(splitToList("1 3 4")))
  println(ok(splitToList("26")))
  println(ok(splitToList("26 4   ")))
  println(ok(splitToList("26 4.4 5   ")))
  println(ok(splitToList("vb")))
  println(ok(splitToList("26 4.4 5  av ")))

}
