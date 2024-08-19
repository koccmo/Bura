package com.bura.states

import com.bura.domain.{Card, CardDesk, Player, Suit}
import com.bura.services.BeatDiscard

import scala.io.StdIn.readLine

trait Defend {
  def defend(
    attackCards: List[Card],
    player: Player,
    cardDesk: CardDesk
  ): BeatDiscard
}

object RobDefend extends Defend {
  def defend(
    attackCard: List[Card],
    rob: Player,
    cardDesk: CardDesk
  ): BeatDiscard = {

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
//
//      if (attackCardTrumps && attackCardAmount > defTrumpAmount) false
//      else if (!attackCardTrumps) attackNoTrump()
//      else defenderBeatsAttacker(attackCard, defTrumpCards)

      if (!attackCardTrumps) attackNoTrump()
      else defenderBeatsAttacker(attackCard, defTrumpCards)
    }

    def bothOneTrump(): BeatDiscard =
      if (defNoTrumpsPointSum <= 10) BeatDiscard(rob, List.empty, defNoTrumpsCards.drop(1))
      else BeatDiscard(rob, defTrumpCards, List.empty)

    def bothTwoTrumps(): BeatDiscard = BeatDiscard(rob, defTrumpCards, List.empty)

    def bothNoTrumps(): BeatDiscard =
      if (attackCardAmount == 1 && cardsDifferentSuit == 3)
        BeatDiscard(rob, defCard.filter(_.suit == attackCardSuit), List.empty) // take
      else if (attackCardAmount == 1 && cardsDifferentSuit == 2)
        BeatDiscard(rob, defCard.filter(_.suit == attackCardSuit).drop(attackCardAmount), List.empty) //take
      else if (attackCardAmount == 2) BeatDiscard(rob, defCard.filter(_.suit == attackCardSuit), List.empty) // take - ?
      else BeatDiscard(rob, defCard, List.empty)                               // need check!

    def defenderHasTrumps(): BeatDiscard = {

      def oneAtcOneTrumpDef(): BeatDiscard =
        if (attackCardsPoints <= 10) BeatDiscard(rob, defNoTrumpsCards.filter(_.suit == attackCardSuit), List.empty) //take
        else BeatDiscard(rob, defTrumpCards, List.empty) // take

      def attackOneDefenderTwoTrump(): BeatDiscard =
        if (attackCardsPoints >= 10) BeatDiscard(rob, defTrumpCards.drop(1), List.empty) // take
        else BeatDiscard(rob, List.empty, defNoTrumpsCards)                              //drop (!defHasAttSuitAndCanBeat)

      def attackTwoDefenderTwoTrump(): BeatDiscard =
//        if (getCardStrength(attackCard).count(_ < getCardStrength(defNoTrumpsCards).sum) >= 1)
        if (getCardStrength(defNoTrumpsCards).exists(defStr => getCardStrength(attackCard).exists(attStr => defStr > attStr)))
          BeatDiscard(rob, defNoTrumpsCards :+ defTrumpCards.minBy(_.rank.strength), List.empty) // take need fix
        else BeatDiscard(rob, defTrumpCards, List.empty)                                         // take

      if (attackCardAmount == 1 && defTrumpCards.size == 1) oneAtcOneTrumpDef()
      else if (attackCardAmount == 1 && defTrumpCards.size == 2) attackOneDefenderTwoTrump()
      else if (attackCardAmount == 2 && defTrumpCards.size == 2) attackTwoDefenderTwoTrump()
      else BeatDiscard(rob, defCard, List.empty)
    }

    def getIfCanBeat(): BeatDiscard =
      if (attackCardTrumps && attackCardAmount == 1 && defTrumpCards.size == 1)
        bothOneTrump() // take off attackCardAmount == 1, make attackCardTrumps.size == 1
      else if (attackCardTrumps && attackCardAmount == 2 && defTrumpCards.size == 2) bothTwoTrumps()
      else if (!attackCardTrumps && defTrumpCards.isEmpty) bothNoTrumps()
      else defenderHasTrumps()

    def cantBeat(): BeatDiscard = {

      def oneAttack(): BeatDiscard =
        if (!defHasTrump && cardsDifferentSuit == 3)
          BeatDiscard(rob, List.empty, defCard.minByOption(_.rank.strength).map(List(_)).getOrElse(List.empty[Card]))
        else if (!defHasTrump && cardsDifferentSuit == 2)
          BeatDiscard(
            rob,
            List.empty,
            defCard.groupBy(_.suit).filter { case (_, cards) => cards.size == 1 }.toList.flatMap { case (_, cards) =>
              cards
            }
          )
        else if (defHasTrump && cardsDifferentSuit == 3 && defNoTrumpsPointSum <= 14)
          BeatDiscard(rob, List.empty, defNoTrumpsCards.drop(attackCardAmount))
        else if (defHasTrump && cardsDifferentSuit == 2 && defTrumpsPointSum >= 10)
          BeatDiscard(rob, List.empty, defNoTrumpsCards.drop(attackCardAmount))
        else BeatDiscard(rob, List.empty, defCard.drop(2))

      def twoAttack(): BeatDiscard =
        if (!defHasTrump && cardsDifferentSuit == 3) BeatDiscard(rob, List.empty, defCard.take(2))
        else if (!defHasTrump && cardsDifferentSuit == 2)
          BeatDiscard(rob, List.empty, defNoTrumpsCards.sortBy(_.rank.strength).take(2))
        else if (defHasTrump && defTrumpAmount == 2)
          BeatDiscard(rob, List.empty, defNoTrumpsCards :+ defTrumpCards.minBy(_.rank.strength))
        else BeatDiscard(rob, List.empty, defNoTrumpsCards)

      attackCardAmount match {
        case 1 => oneAttack()
        case 2 => twoAttack()
        case _ => BeatDiscard(rob, List.empty, defCard)
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
    players: Player, // need change to player
    cardDesk: CardDesk
  ): BeatDiscard = {
    println(s"Trump is : ${cardDesk.trump.getOrElse("None Trump")}")
    println(s"Your cards - ${players.hand.mkString(", ")}")
    println("Choose -> 1 = Beat, 2 = Drop")
    val input = readLine()

    val beatOrDiscard = input match {
      case "1" =>
        println(s"You need choose ${attackCard.size} cards to beat")
        println(s"Choose ${attackCard.size} card -> 1 = ${players.hand.head}, 2 = ${players.hand(1)}, 3 = ${players.hand(2)}")
        val selectCards: String = readLine()

        selectCards match {
          case "1" => BeatDiscard(players, List(players.hand.head), List.empty[Card])
          case "12" => BeatDiscard(players, players.hand.take(2), List.empty[Card])
          case "123" => BeatDiscard(players, players.hand, List.empty[Card])
          case _ => defend(attackCard, players, cardDesk)
        }
      case "2" =>
        println(s"You need choose ${attackCard.size} cards to discard")
        println(s"Choose card -> 1 = ${players.hand.head}, 2 = ${players.hand(1)}, 3 = ${players.hand(2)}")
        val selectCards: String = readLine()

        selectCards match {
          case "1" => BeatDiscard(players, List.empty[Card], List(players.hand.head))
          case "12" => BeatDiscard(players, List.empty[Card], players.hand.take(2))
          case "123" => BeatDiscard(players, List.empty[Card], players.hand)
          case _ => defend(attackCard, players, cardDesk)
        }
      case _   => defend(attackCard, players, cardDesk)
    }

    beatOrDiscard
  }  //need write method if choose 1, 2, 3, 12, 13, 23, 123
}
