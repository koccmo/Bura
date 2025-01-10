package com.bura.services

import com.bura.domain.{Card, Human, Player, Robot, TricksAndHidden}

trait PlayerUpdater {
  def apply(player: Player): Player
}

object PlayerUpdater {

  implicit def addWins(amount: Int): PlayerUpdater = updatePlayer(wins = Some(amount))

  implicit def updateAttack(attack: Boolean): PlayerUpdater = updatePlayer(attack = Some(attack))

  implicit def addTricks(cards: List[Card]): PlayerUpdater = updatePlayer(tricks = Some(cards))

  implicit def tricksAndHidden(tricksAndHidden: TricksAndHidden): PlayerUpdater =
    updatePlayer(tricks = Some(tricksAndHidden.tricks), hiddenTricks = Some(tricksAndHidden.hiddenTricks))

  def updatePlayer(
                    wins: Option[Int] = None,
                    attack: Option[Boolean] = None,
                    tricks: Option[List[Card]] = None,
                    hiddenTricks: Option[List[Card]] = None
                  ): PlayerUpdater = new PlayerUpdater {
    def apply(player: Player): Player = player match {
      case human: Human => human.copy(
        wins = wins.map(_ + human.wins).getOrElse(human.wins),
        attack = attack.getOrElse(human.attack),
        tricks = tricks.map(_ ++ human.tricks).getOrElse(human.tricks),
        hiddenTricks = hiddenTricks.map(_ ++ human.hiddenTricks).getOrElse(human.hiddenTricks)
      )
      case robot: Robot => robot.copy(
        wins = wins.map(_ + robot.wins).getOrElse(robot.wins),
        attack = attack.getOrElse(robot.attack),
        tricks = tricks.map(_ ++ robot.tricks).getOrElse(robot.tricks),
        hiddenTricks = hiddenTricks.map(_ ++ robot.hiddenTricks).getOrElse(robot.hiddenTricks)
      )
    }
  }
}