package com.bura.services

import com.bura.domain.{CardDesk, Human, Player, Robot}
import com.bura.states.{Game, Games}

trait RevealCards {
  def apply(attacker: Player, defender: Player): Games
}

object RevealCards {
  def apply(game: Games): RevealCards = new RevealCards {
    def apply(attacker: Player, defender: Player): Games = {
      val attackerPoints: Int = attacker.allCardsValue
      val defenderPoints: Int = defender.allCardsValue

      if (attackerPoints > defenderPoints) attacker match {
        case human: Human =>
          Game().apply(Robot(attack = false), Human(name = human.name, attack = true), CardDesk().setUpTrump)
        case _: Robot     =>
          Game()
            .apply(Robot(), Human(name = defender.name), CardDesk().setUpTrump) //Todo need make each player gameWins: Int = 0
      }
      else if (defenderPoints > attackerPoints) defender match {
        case human: Human =>
          Game().apply(Robot(attack = false), Human(name = human.name, attack = true), CardDesk().setUpTrump)
        case _: Robot     =>
          Game()
            .apply(Robot(), Human(name = attacker.name), CardDesk().setUpTrump) //Todo need make each player gameWins: Int = 0
      }
      else Game().apply(Robot(), Human(), CardDesk().setUpTrump)
    }
  }
}
