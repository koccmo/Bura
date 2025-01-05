package com.bura.domain

import scala.annotation.tailrec
import scala.util.Random

case class CardDesk(cards: List[Card] = CardDesk.cardsList, trump: Option[Card] = None) {

  val random = new Random()

  def setUpTrump: CardDesk = {
    val newTrump:Card = cards(random.nextInt(cards.size))
    val remainingCards: List[Card] = cards.filter(_ != newTrump)

    CardDesk(remainingCards, Some(newTrump))
  }

//  def setTrump(trumpCard: Card): CardDesk = {
//    val remainingCards: List[Card] = cards.filter(_ != trumpCard)
//    CardDesk(remainingCards, Some(trumpCard))
//  }


  def get(amount: Int): List[Card] = {
    @tailrec
    def helper(
                list: List[Card],
                amount: Int,
                acc: List[Card]
              ): List[Card] =
      (list, amount) match {
        case (Nil, 0) => acc
        case (_, 0) => acc
        case (_, _) =>
          val card = list(random.nextInt(list.size))
          val newAcc = acc :+ card
          val upgradedList = list.filter(_ != card)

          helper(upgradedList, amount - 1, newAcc)
      }

    helper(cards, amount, List.empty[Card]) //Todo need do in this method upgrade CardDesk
  }

  def upgrade(cardsToUpgrade: List[Card]): CardDesk = {
    val newCardList = cards.diff(cardsToUpgrade)

    CardDesk(newCardList, trump)
  }
}

object CardDesk {

  private val ranks: List[Rank] = Rank.ValuesList

  private val suits: List[Suit] = Suit.ValuesList

  private val card: List[(Rank, Suit)] = for {
    rank <- ranks
    suit <- suits
  } yield (rank, suit)

  val cardsList: List[Card] = card.map {
    case (rank, suit) => Card(rank, suit)
  }
}
