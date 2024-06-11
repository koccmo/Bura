package com.bura.states

import com.bura.domain.{Card, CardDesk, Player, Suit}

trait Attack {
  def attack(player: Player, cardDesk: CardDesk): List[Card]
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

object RobAttack extends Attack {

  def attack(robot: Player, cardDesk: CardDesk): List[Card] = {

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


    if (amountTrump == 2) twoTrump(robot.hand)
    else if (amountDifferentSuits == 2) twoSuits()
    else getSmallestPointCard()
  }
}

object PlayerAttack extends Attack {

  def attack(player: Player, cardDesk: CardDesk): List[Card] = {

    println(
      s"Your visible points = ${player.getPoints} -> Do you want carry on game choose - 1, if you want showdown choose - 2"
    )
    val goStop = scala.io.StdIn.readLine() match {
      case "1" =>
        println(s"Trump = ${cardDesk.trump}, choose cards to Attack - Nr 1(${player.hand.head}), Nr 2(${
          player
            .hand(1)
        }), Nr 3(${player.hand(2)})")

        val input: List[Int] = scala.io.StdIn.readLine().split("").toList.map(_.toInt)

        player.hand.zipWithIndex.collect { case (str, index) if input.contains(index + 1) => str }
      case "2" => List.empty[Card]
      case _ => attack(player, cardDesk)
    }

    goStop
  }
}
