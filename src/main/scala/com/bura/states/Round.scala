package com.bura.states

import com.bura.domain.{Card, CardDesk, Player}
import com.bura.services.{DealingCards, DealtCards}

case class Round (players: List[Player], cardDesk: CardDesk) {
  println("Shuffle Cards")


  val dealt: DealtCards = DealingCards(players, cardDesk)

  println("Cards Dealt")


  val attackPlayer: Player = dealt.players.find(_.attack) match {
    case Some(person) => person
  }


  val attackCards: List[Card] = PlayerAttack.attack(attackPlayer, cardDesk)




}

