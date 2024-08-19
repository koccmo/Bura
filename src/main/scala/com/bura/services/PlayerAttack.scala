package com.bura.services

import com.bura.domain.{Card, CardDesk, Human}

object PlayerAttack {

  def attack(player: Human, cardDesk: CardDesk): List[Card] = {

    println(
      s"Your visible points = ${player.getTricksPoints} -> Do you want carry on game choose - 1, if you want showdown choose - 2"
    )
    val goStop = scala.io.StdIn.readLine() match {
      case "1" =>
        println(s"Trump = ${cardDesk.trump}, choose cards to Attack - Nr 1(${player.hand.head}), Nr 2(${player
          .hand(1)}), Nr 3(${player.hand(2)})")

        val input: List[Int] = scala.io.StdIn.readLine().split("").toList.map(_.toInt)

        player.hand.zipWithIndex.collect { case (str, index) if input.contains(index + 1) => str }
      case "2" => List.empty[Card]
      case _   => attack(player, cardDesk)
    }

    goStop
  }
}
