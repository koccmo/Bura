package com.bura.states

import com.bura.domain._
import com.bura.services.Attacker

trait Attack[T <: Player] {
  def attack(player: T, cardDesk: CardDesk): Attacker
}

//object Attack {
//  def apply(): Attack = new Attack {
//    def apply(item: DealtCards): DealtCards = {
//      val player = item.players
//      val attacker = player.filter(_.attack)
//
//      DealtCards(attacker, item.cardDesk)
//    }
//  }
//}
object Attack {
  def attack[T <: Player](player: T, cardDesk: CardDesk)(implicit attacked: Attack[T]): Attacker =
    attacked.attack(player, cardDesk)

  def apply[T <: Player](implicit attack: Attack[T]): Attack[T] = attack

  implicit object RobAttack extends Attack[Robot] {

    override def attack(robot: Robot, cardDesk: CardDesk): Attacker = {

      val suitCardTuples: List[(Suit, List[Card])] = robot.hand.groupBy(_.suit).toList
      val cardsInPack: Int = cardDesk.cards.size
      val trumpSuit: Suit = cardDesk.trump.get.suit
      val containsTrump: Boolean = suitCardTuples.exists { case (suit, _) => suit == trumpSuit }
      val trumpsPoints = robot.hand.filter(_.suit == trumpSuit).map(_.rank.points).sum
      val otherPoint = robot.hand.filter(_.suit != trumpSuit).map(_.rank.points).sum

      def amountTrump: Int = robot.hand.count(_.suit == trumpSuit)

      def amountDifferentSuits: Int = suitCardTuples.size

      def twoTrump(cards: List[Card]): List[Card] = {
        if (trumpsPoints == 21) cards.filter(_.suit == trumpSuit)
        else if (trumpsPoints >= 10) cards.filter(_.suit == trumpSuit)
        else if (trumpsPoints <= 4 & otherPoint >= 10) cards.filter(_.suit == trumpSuit)
        else cards.filter(_.suit != trumpSuit)
      }

      def oneTrump(cards: List[Card]): List[Card] = robot.hand.filter(_.suit != trumpSuit)

      def twoSuits(): List[Card] = suitCardTuples
        .filter { case (_, cards) => cards.size == 2 }
        .flatMap { case (_, cards) => cards }

      def cardWithSmallestStrength(cards: List[Card]): List[Card] =
        if (cards.isEmpty) cards
        else {
          val smallestStrength = cards.map(_.rank.points).min
          val smallestCard = cards.filter(_.rank.points == smallestStrength)

          smallestCard.drop(smallestCard.size - 1)
        }

      def getSmallestPointCard(): List[Card] = {
        if (suitCardTuples.size == 3 & !containsTrump) cardWithSmallestStrength(robot.hand)
        else {
          if (trumpsPoints >= 10) cardWithSmallestStrength(robot.hand.filter(_.suit != trumpSuit))
          else if (trumpsPoints <= 4 & otherPoint >= 20) robot.hand.filter(_.suit == trumpSuit)
          else if (trumpsPoints <= 4 & otherPoint <= 14) cardWithSmallestStrength(robot.hand.filter(_.suit != trumpSuit))
          else if (trumpsPoints == 0 & otherPoint <= 14) cardWithSmallestStrength(robot.hand.filter(_.suit != trumpSuit))
          else cardWithSmallestStrength(robot.hand)
        }
      }


      val attackCards: List[Card] = if (amountTrump == 2) twoTrump(robot.hand)
      else if (amountDifferentSuits == 2) twoSuits()
      else getSmallestPointCard()

      Attacker(robot, attackCards)
    }
  }

  implicit object PlayerAttack extends Attack[Human] {

    override def attack(human: Human, cardDesk: CardDesk): Attacker = {

      val goStop: List[Card] = scala.io.StdIn.readLine() match {
        case "1" =>
          println(s"Trump = ${cardDesk.trump}, choose cards to Attack - Nr 1(${human.hand.head}), Nr 2(${
            human
              .hand(1)
          }), Nr 3(${human.hand(2)})")

          val input: List[Int] = scala.io.StdIn.readLine().split("").toList.map(_.toInt)

          human.hand.zipWithIndex.collect { case (str, index) if input.contains(index + 1) => str }
        case _ => List.empty[Card]
      }

      Attacker(human, goStop)
      //Todo remove fink about Do yo want ti coutined game in Player part!
    }


  }
}



