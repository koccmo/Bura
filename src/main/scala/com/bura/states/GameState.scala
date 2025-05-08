package com.bura.states

import com.bura.domain.{CardDesk, Human, Robot}

trait GameState {
  def human: Human
  def robot: Robot
  def cardDesk: CardDesk
}

case class GameStart(human: Human, robot: Robot, cardDesk: CardDesk) extends GameState

case class GameFinish(human: Human, robot: Robot, cardDesk: CardDesk, winner: Boolean) extends GameState
