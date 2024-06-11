package com.bura.domain

sealed trait Rank {
  def strength: Int
  def points: Int
}

object Rank {

  case object Six extends Rank {
    override def points: Int   = 0
    override def strength: Int = 1
    override def toString      = "Six"
  }

  case object Seven extends Rank {
    override def points: Int      = 0
    override def strength: Int    = 2
    override def toString: String = "Seven"
  }

  case object Eight extends Rank {
    override def points: Int      = 0
    override def strength: Int    = 3
    override def toString: String = "Eight"
  }

  case object Nine extends Rank {
    override def points: Int      = 0
    override def strength: Int    = 4
    override def toString: String = "Nine"
  }

  case object Ten extends Rank {
    override def points: Int      = 10
    override def strength: Int    = 8
    override def toString: String = "Ten"
  }

  case object Jack extends Rank {
    override def points: Int      = 2
    override def strength: Int    = 5
    override def toString: String = "Jack"
  }

  case object Queen extends Rank {
    override def points: Int      = 3
    override def strength: Int    = 6
    override def toString: String = "Queen"
  }

  case object King extends Rank {
    override def points: Int      = 4
    override def strength: Int    = 7
    override def toString: String = "King"
  }

  case object Ace extends Rank {
    override def points: Int      = 11
    override def strength: Int    = 9
    override def toString: String = "Ace"
  }

  val ValuesList: List[Rank]      = List(Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
  val ValuesMap: Map[String, Int] = ValuesList.map(x => x.toString -> x.strength).toMap

}
