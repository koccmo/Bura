package com.bura.services

import com.bura.domain._

import scala.annotation.tailrec
import scala.util.Random

trait DealingCards {

  def apply(players: List[Player], cardDesk: CardDesk): DealtCards
}

object DealingCards {

      def apply(players: List[Player], cardDesk: CardDesk): DealtCards = {

        val upgradedCardDesk = if (cardDesk.trump.isEmpty) cardDesk.doTrump else cardDesk

        def deal(players: List[Player], cards: List[Card]): List[Player] = {

          val neededCardAmount: Int = players.map {
            case x: Human => x.needCard
            case x: Robot => x.needCard
          }.sum / players.size

          val shuffledCards = Random.shuffle(cards).grouped(neededCardAmount).toList

          def helper(players: List[Player], cards: List[List[Card]]): List[Player] = {
            @tailrec
            def helper(
              players: List[Player],
              cards: List[List[Card]],
              acc: List[Player]
            ): List[Player] =
              if (players.isEmpty) acc
              else helper(players.tail, cards.tail, acc :+ players.head.setHand(cards.head))

            helper(players, cards, List.empty[Player])
          }

          helper(players, shuffledCards)
        }

        val upgradedPlayer: List[Player] = deal(players, upgradedCardDesk.cards)

        val dealtCardToPlayers: List[Card] = upgradedPlayer.flatMap(_.hand)

        DealtCards(upgradedPlayer, upgradedCardDesk.upgrade(dealtCardToPlayers))
      }

}
