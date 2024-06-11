package com.bura.states

import com.bura.domain.{Human, Player, Robot}

object CreatePlayer {
  def create: List[Player] = {
    println("Hello do you want play? If yes Enter your name - ")
    val name = scala.io.StdIn.readLine()
    val human = Human(name)
    val robot: List[Player] = List(Robot())

    robot :+ human
  }
}
