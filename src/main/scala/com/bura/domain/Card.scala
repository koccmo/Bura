package com.bura.domain

final case class Card(rank: Rank, suit: Suit) {
  override def toString: String = s"${rank.toString}-${suit.toString}"
}


