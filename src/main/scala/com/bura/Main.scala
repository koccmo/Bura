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
    val game: Game = Game()
    game.apply(robot, human, cardDesk)
  }
}
