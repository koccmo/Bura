package com.bura

import com.bura.domain.Rank.{Ace, Six}
import com.bura.domain.Suit.{Clubs, Diamonds, Hearts, Spades}
import com.bura.domain.{Card, CardDesk, Human, Robot, Suit}
import com.bura.services.RobAttack

import scala.annotation.tailrec
import scala.util.Random

object Test extends App {

  val random = new Random()

  def get(amount: Int): List[Card] = {

    val random = new Random()
    val cards  = CardDesk.cardsList

    @tailrec
    def helper(
      list: List[Card],
      amount: Int,
      acc: List[Card]
    ): List[Card] =
      (list, amount) match {
        case (Nil, 0) => acc
        case (_, 0) => acc
        case (_, _)  =>
          val card = list(random.nextInt(list.size))
          val newAcc = acc :+ card
          val upgradedList = list.filter(_ != card)

          helper(upgradedList, amount - 1, newAcc)
      }
    helper(cards, amount, List.empty[Card])
  }

  println(get(10))

  val cards = CardDesk.cardsList
  println(cards)
  println("-----------")
  val shuffledCards = random.shuffle(cards)
  println(shuffledCards)

  println(cards.diff(shuffledCards))

  val player = Robot()

  println(List(Human("Alex"), Robot()).find(_.attack))

  val newDesc = CardDesk().setUpTrump
  println(newDesc.trump)
  println(newDesc.cards.size)

  val grouperd = List(Card(Ace, Hearts), Card(Ace, Diamonds), Card(Six, Spades)).groupBy(_.suit)
  println(grouperd.contains(Hearts))

  val listCards = List(Card(Ace, Hearts), Card(Ace, Diamonds), Card(Six, Spades))
  println(listCards.exists(_.suit == Hearts))

  println("======================")

  val tuples = listCards.groupBy(_.suit).toList
  val suit: Suit = Clubs

  def containsSuit(tuples: List[(Suit, List[Card])], pred: Suit => Boolean): Boolean =
    tuples.exists { case (suit, _) => pred(suit) }

  println(containsSuit(tuples, _ == suit))
  println("++++++++++++++++++++++++++")


  val cardssss = List(Card(Ace, Hearts), Card(Ace, Diamonds), Card(Six, Spades))
  val cardDesk = CardDesk().setUpTrump
  val myRob = Robot().setHand(cardDesk.get(3))
  val upgradeCardDesc = cardDesk.upgrade(myRob.hand)

  println(myRob.hand)
  println(cardDesk.trump)
  println(upgradeCardDesc.trump)
  println(upgradeCardDesc.cards.size)

  val robAttack = RobAttack
  println(RobAttack.attack(myRob, upgradeCardDesc))
}
