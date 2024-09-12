package com.bura.services

import com.bura.domain._
import com.bura.states.{Attack, GameFinish, GameStart, GameState}
trait Game {
  def apply(gameState: GameState): Game
}

object Game {
  def apply(): Game = new Game {
    def apply(gameState: GameState): Game = {

      val classOfGameStart  = classOf[GameStart]
      val classOfGameFinish = classOf[GameFinish]

      val human: Human       = gameState.human
      val robot: Robot       = gameState.robot
      val cardDesk: CardDesk = gameState.cardDesk

      val dealtCards: DealtCards = DealingCards(List(human, robot), cardDesk)
      val ok = dealtCards.players.find(_.attack)

      val cardList = ok.map {
        case x: Human if x.attack => Attack.attack(x, cardDesk)
        case x: Robot if x.attack => Attack.attack(x, cardDesk)
      }

      new Game
    }
  }

}
