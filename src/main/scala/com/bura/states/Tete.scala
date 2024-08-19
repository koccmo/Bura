package com.bura.states

import com.bura.domain.{CardDesk, Human, Player, Robot}
import com.bura.services.DealingCards

object Tete extends App {

  val name: String = "All"
  val human: Player = Human(name)
  val robot: Player = Robot()
  val cardDesk: CardDesk = CardDesk()
  val players: List[Player] = List(robot, human)
  val dealtCards = DealingCards(players, cardDesk)
  val newCardDesk = dealtCards.cardDesk
  val playersDealt = dealtCards.players
  val playersList: List[Player] = playersDealt.collect {
    case robot@Robot(_, _, _, _, _, true) => robot
    case human@Human(_, _, _, _, _, true) => human
  }
  val attack = playersList.filter(_.attack)
  val defend = playersList.filter(!_.attack)

  val attClass: Player = attack.head
  val classOfRobot = classOf[Robot]

  println(attClass.getClass.equals(classOfRobot))
  println(robot.getClass)
  println(Robot.getClass)
}
