package com.bura.domain

import com.bura.services.{Attack, PlayerDefend, RobDefend}

trait Round {
  def apply(attacker: Player, defender: Player, cardDesk: CardDesk): RoundEnd
}

case class RoundEnd(
  attacker: Player,
  defender: Player,
  cardDesk: CardDesk
) extends Round

object Round {
  def apply(): Round = new Round {
    def apply(
               attacker: Player,
               defender: Player,
               cardDesk: CardDesk
             ): RoundEnd = {

      val attackerCard: List[Card] = attacker match {
        case human: Human => Attack.attack(human, cardDesk)
        case robot: Robot => Attack.attack(robot, cardDesk)
      }

      val defend: BeatDiscard = defender match {
        case human: Human => PlayerDefend.defend(attackerCard, human, cardDesk)
        case robot: Robot => RobDefend.defend(attackerCard, robot, cardDesk)
      } //Todo need change name of val !

      object Validate {
        def canBeat(attackCard: List[Card], defendCard: List[Card]): Boolean = true
      } //Todo need write Validation!

      if (defend.hiddenDiscard.isEmpty && Validate.canBeat(attackerCard, defend.defendCards)) {
        val newAttacker: Player = defend.player match {
          case human: Human => human.copy(
            tricks = human.tricks ++ attackerCard ++ defend.defendCards,
            attack = true
          )
          case robot: Robot => robot.copy(
            tricks = robot.tricks ++ attackerCard ++ defend.defendCards,
            attack = true
          )
        }

        val newDefender: Player = attacker match {
          case human: Human => human.copy(attack = false)
          case robot: Robot => robot.copy(attack = false)
        }

        RoundEnd(newAttacker, newDefender, cardDesk)
      } else if (defend.hiddenDiscard.nonEmpty) {
        val newAttacker: Player = attacker match {
          case human: Human =>
            human.copy(tricks = human.tricks ++ attackerCard,
              hiddenTricks = human.hiddenTricks ++ defend.hiddenDiscard
            )
          case robot: Robot =>
            robot.copy(tricks = robot.tricks ++ attackerCard,
              hiddenTricks = robot.hiddenTricks ++ defend.hiddenDiscard
            )
        }

        RoundEnd(newAttacker, defend.player, cardDesk)
      } else {
        val newAttacker: Player = attacker match {
          case human: Human =>
            human.copy(
              tricks = human.tricks ++ attackerCard,
              hiddenTricks = human.hiddenTricks ++ defend.defendCards
            )
          case robot: Robot =>
            robot.copy(
              tricks = robot.tricks ++ attackerCard,
              hiddenTricks = robot.hiddenTricks ++ defend.defendCards
            )
        }

        RoundEnd(
          newAttacker,
          defend.player,
          cardDesk
        ) //Todo need check if Hand cards is upgrated after this!Minus attackerCard and minus DefenderCards!
      }
    }
  }
}
