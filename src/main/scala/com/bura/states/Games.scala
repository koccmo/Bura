package com.bura.states

import com.bura.domain.{CardDesk, Player, Winner}
import com.bura.services.{RevealCards, Round, RoundEnd, Win}

trait Games {
  def apply(
    attacker: Player,
    defender: Player,
    cardDesk: CardDesk
  ): Games
}

object Games {
  def apply(
    revealCards: RevealCards,
    round: Round,
    win: Win
  ): Games = new Games {
    def apply(
      attacker: Player,
      defender: Player,
      cardDesk: CardDesk
    ): Games = {

      val attackerCards         = cardDesk.get(attacker.needCard)
      val cardDeskAfterAttacker = cardDesk.upgrade(attackerCards)
      val defenderCards         = cardDesk.get(defender.needCard)
      val cardDeskAfterDefender = cardDesk.upgrade(defenderCards)
      val newCardDesk           = cardDeskAfterDefender.setUpTrump //Todo need write Card Dealing!Proper!

      val roundEnd: RoundEnd = round(
        attacker.setHand(attackerCards),
        defender.setHand(defenderCards),
        newCardDesk
      ) //Todo need refactor this!!!

      val winner: Winner = win(roundEnd.attacker)

      if (winner.win)
        Games(revealCards, round, win)(Player.upgrade(1, roundEnd.attacker), Player.upgrade(0, roundEnd.defender), new CardDesk())
      else Games(revealCards, round, win)(roundEnd.attacker, roundEnd.defender, newCardDesk)

      if (newCardDesk.cards.length < roundEnd.attacker.needCard * 2) {
        val (newAttacker, newDefender) = revealCards(attacker, defender)
        Games(revealCards, round, win)(newAttacker, newDefender, new CardDesk())
      }
      else Games(revealCards, round, win)(roundEnd.attacker, roundEnd.defender, roundEnd.cardDesk)
    }
  }

}
