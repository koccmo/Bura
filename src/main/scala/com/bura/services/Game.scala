package com.bura.services

import com.bura.domain._
import com.bura.states.CreatePlayer

object Game {
  val creator: List[Player] = CreatePlayer.create

  val dealtCards: DealtCards = DealingCards(creator, CardDesk())//

}
