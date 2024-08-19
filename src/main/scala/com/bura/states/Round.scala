package com.bura.states

import com.bura.domain.{CardDesk, Player}

trait Round {
  def attacker: Player

  def defender: Player

  def cardDesk: CardDesk
}

case class RoundStart(
  attacker: Player,
  defender: Player,
  cardDesk: CardDesk
) extends Round

case class RoundEnd(
  attacker: Player,
  defender: Player,
  cardDesk: CardDesk
) extends Round
