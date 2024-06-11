package com.bura.domain

trait Player {
  def setHand(listCard: List[Card]): Player
  def hand: List[Card]
  def attack: Boolean
  def trickedCards: List[Card] = List.empty[Card]
  def getPoints: Int
}

case class Human(
  name: String,
  playerPoint: Int = 0,
  hand: List[Card] = List.empty[Card],
  hiddenTrickedCard: List[Card] = List.empty[Card],
  attacker: Boolean = false
) extends Player {
  def needCard: Int = Player.Quantity - hand.size

  def getPoints: Int = trickedCards.map(_.rank.points).sum

  def setHand(listCard: List[Card]): Human = Human(name, playerPoint, hand ++ listCard)

  def attack: Boolean = attacker

  def setPoints(points: Int): Human = Human(name, playerPoint + points)

}

case class Robot(
  hand: List[Card] = List.empty[Card],
  robotPoints: Int = 0,
  trick: List[Card] = List.empty,
  hiddenTrick: List[Card] = List.empty,
  attacker: Boolean = true
) extends Player {

  def setHand(cards: List[Card]): Robot = Robot(hand ++ cards, 0, trick, List.empty, attacker)

  def attack: Boolean = attacker

  def setAttack(itAttack: Boolean): Robot = Robot(hand, robotPoints, trick, hiddenTrick, itAttack)

  def setHiddenTrick(cards: List[Card]): Robot = Robot(hand, robotPoints, trick, hiddenTrick ++ cards)

  def trickValue: Int = trick.map(_.rank.strength).sum

  def allCardValue: Int = (trick ++ hiddenTrick).map(_.rank.strength).sum

  def needCard: Int = Player.Quantity - hand.size

  def getPoints: Int = trickedCards.map(_.rank.points).sum
}

object Player {
  val Quantity: Int = 3
}
