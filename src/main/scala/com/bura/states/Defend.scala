package com.bura.states

import com.bura.domain.{Card, CardDesk, Player, Suit}

import scala.io.StdIn.readLine

trait Defend {
  def defend(
    attackCard: List[Card],
    players: Player,
    cardDesk: CardDesk
  ): List[Card]
}

object RobDefend extends Defend {
  def defend(
    attackCard: List[Card],
    rob: Player,
    cardDesk: CardDesk
  ): List[Card] = {

    val trumpSuit: Suit = cardDesk.trump.get.suit

    val attackCardAmount: Int     = attackCard.size
    val attackCardTrumps: Boolean = attackCard.map(_.suit).contains(trumpSuit)
    val attackCardSuit: Suit      = attackCard.map(_.suit).head
    val attackCardsPoints: Int    = attackCard.map(_.rank.points).sum

    val defCard: List[Card]          = rob.hand.sortBy(_.rank.strength)
    val defTrumpAmount: Int          = defCard.count { card => card.suit == trumpSuit }
    val defTrumpCards: List[Card]    = defCard.filter(_.suit == trumpSuit)
    val defNoTrumpsCards: List[Card] = defCard.diff(defTrumpCards).sortBy(_.rank.strength).reverse
    val defNoTrumpsPointSum: Int     = defCard.diff(defTrumpCards).map(_.rank.points).sum
    val defTrumpsPointSum: Int       = defTrumpCards.map(_.rank.points).sum
    val defHasTrump: Boolean         = defCard.exists(_.suit == trumpSuit)
    val cardsDifferentSuit: Int      = defCard.groupBy(_.suit).toList.size

    def getCardStrength(cards: List[Card]): List[Int] = cards.map(_.rank.strength).sorted

    val defHasAttSuitAndCanBeat: Boolean = {

      val cardSimilarSuitAttack: List[Card] = defNoTrumpsCards.filter(_.suit == attackCardSuit)

      getCardStrength(cardSimilarSuitAttack).exists(defStr =>
        getCardStrength(attackCard).exists(attStr => defStr > attStr)
      )
    }

    val canBeat: Boolean = {

      def defenderBeatsAttacker(attackerCard: List[Card], defenderCards: List[Card]): Boolean = {
        val attackCardsStrength   = getCardStrength(attackerCard)
        val defenderCardsStrength = getCardStrength(defenderCards)

        def similarCardsAmount: Boolean = {
          val countBeats = defenderCardsStrength.map { strength => attackCardsStrength.count(_ < strength) }.sum

          attackerCard.size match {
            case 1 if countBeats >= 1 => true
            case 2 if countBeats >= 3 => true
            case 3 if countBeats >= 6 => true
            case _                    => false
          }
        }

        def defenderHasMoreCards: Boolean = {
          val defenderCombinationAsAttackerAmount: List[List[Card]] =
            defenderCards.combinations(attackerCard.size).toList
          val ok: List[Int]                                         =
            defenderCombinationAsAttackerAmount
              .map(getCardStrength)
              .foldLeft(List.empty[Int]) { (acc, innerList) =>
                val counts = innerList.map { strength => attackCardsStrength.count(_ < strength) }

                acc ++ counts
              }

          ok.contains(attackerCard.size)
        }

        if (attackerCard.isEmpty) true
        else if (attackerCard.size == defenderCards.size) similarCardsAmount
        else defenderHasMoreCards
      }

      def attackNoTrump(): Boolean = {
        val attackerCardMinusTrumps         = attackCard.sortBy(_.rank.strength).dropRight(defTrumpAmount)
        val defenderCardNoTrump: List[Card] = defCard
          .filter(card => card.suit != trumpSuit && card.suit == attackCardSuit)

        defenderBeatsAttacker(attackerCardMinusTrumps, defenderCardNoTrump)
      }

      if (attackCardTrumps && attackCardAmount > defTrumpAmount) false
      else if (!attackCardTrumps) attackNoTrump()
      else defenderBeatsAttacker(attackCard, defTrumpCards)
    }

    //    def cardIfCan(): List[Card] = {
    //      if (attackCardTrumps)
    //    }

    //    def attTrumpsBeaten(): List[Card] = {
    //      if (attackCardAmount == 2) defTrumpCards
    //      else if (attackCardAmount == 1 && attackCardsPoints < 10
    //        && defTrumpCards.size == 2 && defTrumpsPointSum >= 10 && defNoTrumpsPointSum <= 10 ) defNoTrumpsCards
    //      else if (attackCardAmount == 1 && attackCardsPoints == 10) defTrumpCards.filter(_.rank.points == 11)
    //      else if (attackCardAmount == 1 && attackCardsPoints >= 10 && defTrumpCards.size == 2 && defNoTrumpsPointSum < 10)
    //        defNoTrumpsCards // it should be dropped cards
    //      else if (attackCardAmount == 1 && attackCardsPoints == 0 && defTrumpCards.size == 2 && defNoTrumpsPointSum == 0) defNoTrumpsCards // it should be dropped cards
    //      else if (attackCardAmount == 1 && a)
    //    }

    def bothOneTrump(): List[Card] =
      if (attackCardsPoints == 0 && defTrumpsPointSum == 0 && defNoTrumpsPointSum == 21) defTrumpCards //take
      else if (attackCardsPoints == 0 && defTrumpsPointSum == 0 && cardsDifferentSuit == 2) defTrumpCards //take
      else if (attackCardsPoints == 0 && defTrumpsPointSum == 0 && defNoTrumpsPointSum <= 15 && cardsDifferentSuit == 3)
        defNoTrumpsCards.drop(attackCardAmount) // drop
      else if (attackCardsPoints == 0 && defTrumpsPointSum >= 2 && defTrumpsPointSum <= 4 && cardsDifferentSuit == 2)
        defTrumpCards //take
      else if (attackCardsPoints == 0 && defTrumpsPointSum >= 2 && defTrumpsPointSum <= 4 && cardsDifferentSuit == 3)
        defNoTrumpsCards.drop(attackCardAmount) //drop
      else if (attackCardsPoints == 0 && defTrumpsPointSum <= 10 && defNoTrumpsPointSum == 21) defTrumpCards //take
      else if (
        attackCardsPoints == 0 && defTrumpsPointSum <= 10 && defNoTrumpsPointSum <= 15 && cardsDifferentSuit == 2
      ) defTrumpCards //take
      else if (
        attackCardsPoints == 0 && defTrumpsPointSum <= 10 && defNoTrumpsPointSum <= 15 && cardsDifferentSuit == 3
      ) defNoTrumpsCards.drop(attackCardAmount) //drop
      //      else if (attackCardsPoints == 0 && defTrumpsPointSum <= 10 && defNoTrumpsPointSum == 0 && cardsDifferentSuit == 2) defTrumpCards //take
      //      else if (attackCardsPoints == 0 && defTrumpsPointSum <= 10 && defNoTrumpsPointSum == 0 && cardsDifferentSuit == 3) defTrumpCards //take
      else if (attackCardsPoints >= 2 && defNoTrumpsPointSum == 21) defTrumpCards //take
      else if (attackCardsPoints >= 2 && cardsDifferentSuit == 2) defTrumpCards //take
      else if (attackCardsPoints >= 2 && defNoTrumpsPointSum <= 15 && cardsDifferentSuit == 2) defTrumpCards //take
      else if (attackCardsPoints >= 2 && defNoTrumpsPointSum <= 15 && cardsDifferentSuit == 3)
        defNoTrumpsCards.drop(attackCardAmount) //drop
      else List.empty

    def bothTwoTrumps(): List[Card] = defTrumpCards

    def bothNoTrumps(): List[Card] =
      if (attackCardAmount == 1 && cardsDifferentSuit == 3) defCard.filter(_.suit == attackCardSuit) // take
      else if (attackCardAmount == 1 && cardsDifferentSuit == 2)
        defCard.filter(_.suit == attackCardSuit).drop(attackCardAmount) //take
      else if (attackCardAmount == 2) defCard.filter(_.suit == attackCardSuit)
      else defCard

    def defenderHasTrumps(): List[Card] = {
      def oneAtcOneTrumpDef(): List[Card] =
        if (attackCardsPoints == 11 && cardsDifferentSuit == 3 && defNoTrumpsPointSum >= 20) defTrumpCards // take
        else if (attackCardsPoints == 10 && cardsDifferentSuit == 3 && defHasAttSuitAndCanBeat)
          defNoTrumpsCards.filter(_.suit == attackCardSuit) //take
        else if (attackCardsPoints <= 4 && cardsDifferentSuit == 3 && defHasAttSuitAndCanBeat)
          defNoTrumpsCards.filter(_.suit == attackCardSuit) //take
        else defTrumpCards

      def attackOneDefenderTwoTrump(): List[Card] =
        if (defHasAttSuitAndCanBeat) defNoTrumpsCards // take
        else defNoTrumpsCards                         //drop (!defHasAttSuitAndCanBeat)

      def attackTwoDefenderOneTrump(): List[Card] = {
        val amountCanBeatNotTrump: Int = {
          val strengthOfSimpleDefCard = defNoTrumpsCards.map(_.rank.strength).sum

          getCardStrength(attackCard).count(_ < strengthOfSimpleDefCard)
        }

        if (cardsDifferentSuit == 3 && defHasAttSuitAndCanBeat)
          defNoTrumpsCards.filter(_.suit == attackCardSuit) ++ defTrumpCards             // take
        else if (cardsDifferentSuit == 2 && amountCanBeatNotTrump == 4) defNoTrumpsCards // take
        else if (cardsDifferentSuit == 2 && amountCanBeatNotTrump == 3) defNoTrumpsCards // take
        else if (cardsDifferentSuit == 2 && amountCanBeatNotTrump == 2)
          defTrumpCards :+ defNoTrumpsCards.maxBy(_.rank.strength) // take
        else if (cardsDifferentSuit == 2 && amountCanBeatNotTrump == 1)
          defTrumpCards :+ defNoTrumpsCards.maxBy(_.rank.strength) // take
        else defTrumpCards :+ defNoTrumpsCards.maxBy(_.rank.strength)                    //take
      }

      def attackTwoDefenderTwoTrump(): List[Card] =
        if (getCardStrength(attackCard).count(_ < getCardStrength(defNoTrumpsCards).sum) >= 1)
          defNoTrumpsCards :+ defTrumpCards.minBy(_.rank.strength) // take need fix
        else defTrumpCards                                         // take

      if (attackCardAmount == 1 && defTrumpCards.size == 1) oneAtcOneTrumpDef()
      else if (attackCardAmount == 1 && defTrumpCards.size == 2) attackOneDefenderTwoTrump()
      else if (attackCardAmount == 2 && defTrumpCards.size == 1) attackTwoDefenderOneTrump()
      else if (attackCardAmount == 2 && defTrumpCards.size == 2) attackTwoDefenderTwoTrump()
      else defCard
    }

    def getIfCanBeat(): List[Card] =
      if (attackCardTrumps && attackCardAmount == 1 && defTrumpCards.size == 1) bothOneTrump()
      else if (attackCardTrumps && attackCardAmount == 2 && defTrumpCards.size == 2) bothTwoTrumps()
      else if (!attackCardTrumps && defTrumpCards.isEmpty) bothNoTrumps()
      else defenderHasTrumps()

    def cantBeat(): List[Card] = {

      def oneAttack(): List[Card] =
        if (!defHasTrump && cardsDifferentSuit == 3)
          defCard.minByOption(_.rank.strength).map(List(_)).getOrElse(List.empty[Card])
        else if (!defHasTrump && cardsDifferentSuit == 2)
          defCard.groupBy(_.suit).filter { case (_, cards) => cards.size == 1 }.toList.flatMap { case (_, cards) =>
            cards
          }
        else if (defHasTrump && cardsDifferentSuit == 3 && defNoTrumpsPointSum <= 14)
          defNoTrumpsCards.drop(attackCardAmount)
        else if (defHasTrump && cardsDifferentSuit == 2 && defTrumpsPointSum >= 10)
          defNoTrumpsCards.drop(attackCardAmount)
        else defCard.drop(2)

      def twoAttack(): List[Card] =
        if (!defHasTrump && cardsDifferentSuit == 3) defCard.drop(1)
        else if (!defHasTrump && cardsDifferentSuit == 2) defNoTrumpsCards.sortBy(_.rank.strength).take(2)
        else if (defHasTrump && defTrumpAmount == 2) defNoTrumpsCards :+ defTrumpCards.minBy(_.rank.strength)
        else defNoTrumpsCards

      attackCardAmount match {
        case 1 => oneAttack()
        case 2 => twoAttack()
        case _ => defCard
      }
    }

    println(canBeat)
    if (canBeat) getIfCanBeat()
    else cantBeat()
  }
}

object PlayerDefend extends Defend {
  def defend(
    attackCard: List[Card],
    players: Player,
    cardDesk: CardDesk
  ): List[Card] = {
    println(s"Attacked card - $attackCard")
    println(s"Your cards - ${players.hand}")
    println("Choose -> 1 = Beat, 2 = Drop")
    val beatDrop = scala.io.StdIn.readLine()
    beatDrop match {
      case "1" =>
        println(s"Choose card -> 1 = ${players.hand(1)}, 2 = ${players.hand(2)}, 3 = ${players.hand(3)}")
        val selectCard: String = readLine()
      case "1" =>
        println(s"Choose card -> 1 = ${players.hand(1)}, 2 = ${players.hand(2)}, 3 = ${players.hand(3)}")
        val selectCard: String = readLine()
      case _   => defend(attackCard, players, cardDesk)
    }

    List.empty[Card]
  }
}
