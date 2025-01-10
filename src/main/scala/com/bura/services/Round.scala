package com.bura.services

import com.bura.domain._

trait Round {
  def apply(
    attacker: Player,
    defender: Player,
    cardDesk: CardDesk
  ): RoundEnd
}

case class RoundEnd(
  attacker: Player,
  defender: Player,
  cardDesk: CardDesk
)

object Round {
  def apply(): Round = new Round {
    def apply(
      attacker: Player,
      defender: Player,
      cardDesk: CardDesk
    ): RoundEnd = {

      val attackerCard: List[Card] = attacker match {
        case human: Human => Attack.attack(human, cardDesk)
        case robot: Robot =>
          println(s"The opponent attacks you with ${Attack.attack(robot, cardDesk).mkString(", ")}")

          Attack.attack(robot, cardDesk)
      }

      val defend: BeatDiscard = defender match {
        case human: Human => PlayerDefend.defend(attackerCard, human, cardDesk)
        case robot: Robot => RobDefend.defend(attackerCard, robot, cardDesk)
      } //Todo need change name of val !

      object Validate {
        def canBeat(attackCard: List[Card], defendCard: List[Card]): Boolean = true
      } //Todo need write Validation!

      if (defend.hiddenDiscard.isEmpty && Validate.canBeat(attackerCard, defend.defendCards)) {
        val newAttacker: Player = Player.upgrade(attackerCard ++ defend.defendCards, true, defend.player)
        val newDefender: Player = Player.upgrade(false, attacker)

        RoundEnd(newAttacker, newDefender, cardDesk)
      } else if (defend.hiddenDiscard.nonEmpty) {
        val newAttacker: Player = Player.upgrade(TricksAndHidden(attackerCard, defend.hiddenDiscard), attacker)

        RoundEnd(newAttacker, defend.player, cardDesk)
      } else {
        val newAttacker: Player = Player.upgrade(attackerCard ++ defend.defendCards, attacker)

        RoundEnd(
          newAttacker,
          defend.player,
          cardDesk
        ) //Todo need check if Hand cards is upgrated after this!Minus attackerCard and minus DefenderCards!
      }
    }
  }
}
