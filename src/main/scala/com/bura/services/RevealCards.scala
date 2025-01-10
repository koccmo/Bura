package com.bura.services

import com.bura.domain.{CardDesk, Human, Player, Robot}
import com.bura.states.{Game, Games}

trait RevealCards {
  def apply(attacker: Player, defender: Player): (Player, Player)
}

object RevealCards {
  def apply(): RevealCards = new RevealCards {
    def apply(attacker: Player, defender: Player): (Player, Player) = {
      val attackerPoints: Int = attacker.allCardsValue
      val defenderPoints: Int = defender.allCardsValue

      if (attackerPoints > defenderPoints) (Player.upgrade(1, attacker), defender)//Todo need make each player gameWins: Int = 0
      else (Player.upgrade(1, defender), attacker)
    }
  }
}
