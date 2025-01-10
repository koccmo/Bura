package com.bura

import com.bura.domain._
import com.bura.services.{RevealCards, Round, Win}
import com.bura.states.{Game, Games}

object Main {

  def main(args: Array[String]): Unit = {
    println("Welcome in Game!")
    println("Enter your name -")
    val name: String       = io.StdIn.readLine()
    val human: Human       = Human(name)
    val robot: Robot       = Robot()
    val cardDesk: CardDesk = CardDesk().setUpTrump
    val revealCards: RevealCards = RevealCards()
    val round: Round = Round()
    val win: Win = Win()
    val game: Games        = Games(revealCards, round, win)

    game(robot, human, cardDesk)

  }

}
