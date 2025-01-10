package com.bura.services

import com.bura.domain.{Human, Player, Robot, Winner}

trait Win {
 def apply(attackPlayer: Player): Winner
}

object Win {
  def apply(): Win = new Win {

    def apply(attackPlayer: Player): Winner = attackPlayer match {
      case _: Human =>
        println(s"Our tricks points = ${attackPlayer.trickValue}")
        println("Choose : 1 = Carry on game! 2 = Reveal cards!")
        val selectedAction: String = io.StdIn.readLine()

        selectedAction match {
          case "1" => new Winner(attackPlayer)
          case "2" =>
            if (attackPlayer.allCardsValue >= 30) new Winner(attackPlayer, true)
            else new Winner(attackPlayer)
        }

      case robot: Robot if robot.getTricksPoints >= 30 => new Winner(attackPlayer, true)
    }

  }
}
