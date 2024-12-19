package com.bura

import com.bura.domain._
import com.bura.states.Game

object Main {

  def main(args: Array[String]): Unit = {
    println("Welcome in Game!")
    println("Enter your name -")
    val name: String = io.StdIn.readLine()
    val human: Human = Human(name)
    val robot: Robot = Robot()
    val cardDesk: CardDesk = CardDesk().setUpTrump
    println(s"TRUMP SUIT IS : ${cardDesk.trump.get.suit}")
    val game: Game = Game()
    game play (robot, human, cardDesk)
  }
}
