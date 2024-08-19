package com.bura.domain

trait Player {
  def name: String
  def setHand(listCard: List[Card]): Player
  def hand: List[Card]
  def attack: Boolean
  def tricks: List[Card]

  def hiddenTricks: List[Card]
  def getTricksPoints: Int = tricks.map(_.rank.points).sum
  def needCard: Int = Player.Quantity - hand.size
  def trickValue: Int = tricks.map(_.rank.strength).sum

  def allCardsValue: Int = (tricks ++ hiddenTricks).map(_.rank.strength).sum
}

case class Human(
  name: String,
  hand: List[Card] = List.empty[Card],
  points: Int = 0,
  tricks: List[Card] = List.empty[Card],
  hiddenTricks: List[Card] = List.empty[Card],
  attack: Boolean = false
) extends Player {

  def setHand(listCard: List[Card]): Human = Human(name, hand ++ listCard, attack = this.attack)

  def setPoints(amount: Int): Human = Human(name, hand, points + amount)


}

case class Robot(
  name:String = "Robot",
  hand: List[Card] = List.empty[Card],
  points: Int = 0,
  tricks: List[Card] = List.empty,
  hiddenTricks: List[Card] = List.empty,
  attack: Boolean = true
) extends Player {

  def setHand(cards: List[Card]): Robot = Robot(name, hand ++ cards, attack = this.attack)


  def setAttack(itAttack: Boolean): Robot = Robot(name, hand, points, tricks, hiddenTricks, itAttack)

//  def setTricked(cards: List[Card]): Robot = Robot(name, hand, points, cards, hiddenTricks, attack)
//
//  def setHiddenTrick(cards: List[Card]): Robot = Robot(name, hand, points, tricks, hiddenTricks ++ cards)
}

object Player {
  val Quantity: Int = 3
}
