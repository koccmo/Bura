package com.bura.domain

sealed trait Suit

object Suit {

  case object Hearts extends Suit { override def toString: String = "Hearts" }

  case object Clubs extends Suit { override def toString: String = "Clubs" }

  case object Spades extends Suit { override def toString: String = "Spades" }

  case object Diamonds extends Suit { override def toString: String = "Diamonds" }

  val ValuesList: List[Suit] = List(Hearts, Clubs, Spades, Diamonds)
  val ValuesMap: Map[Suit, String] = ValuesList.map(x => x -> x.toString).toMap
}


