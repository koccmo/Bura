package com.bura.states

import com.bura.domain._
import com.bura.services.BeatDiscard

case class Game() {
  def apply(robot: Robot, human: Human, cardDesk: CardDesk): Game = {

//    println(s"Robot attack -> ${robot.attack}")
//    println(s"Human attack -> ${human.attack}")
//    println("---------------")
    //    val players: List[Player]      = List(robot, human)
    //    val dealtCards                 = DealingCards(players, cardDesk)
    //    val newCardDesk                = dealtCards.cardDesk
    //    val playersDealt               = dealtCards.players
    val humanCards: List[Card] = cardDesk.get(human.needCard)
    val robotCards: List[Card] = cardDesk.get(robot.needCard)
    val dealtHuman: Human = human.setHand(humanCards)
    val dealtRobot: Robot = robot.setHand(robotCards)
    val upgradedCardDesk = cardDesk.upgrade(humanCards ++ robotCards)
//    println(s"DealtRobot attack -> ${dealtRobot.attack}")
//    println(s"DealtHuman attack -> ${dealtHuman.attack}")
//    println("")
    val attackedCard: List[Card] = if (dealtRobot.attack) {
      val cards = Attack.attack(dealtRobot, upgradedCardDesk)
      println(s"Robot attack cards -> ${cards.mkString(", ")}")

      cards
    } else {
      val cards = Attack.attack(dealtHuman, upgradedCardDesk)
      println(s"${dealtHuman.name} attack cards -> $cards") //Todo println need println after Trump!!!!

      cards
    }

    val attackerPlayer: Player = if (dealtRobot.attack) dealtRobot else dealtHuman
    val defendPlayer: Player = if (dealtHuman.attack) dealtRobot else dealtHuman
//    println(s"Attack player -> $attackerPlayer")
//    println(s"DefendPlayer -> $defendPlayer")

    val defends: BeatDiscard = if (!dealtRobot.attack) {
      RobDefend.defend(attackedCard, dealtRobot, upgradedCardDesk)
    } else PlayerDefend.defend(attackedCard, dealtHuman, upgradedCardDesk)

    object Validate {
      def canBeat(attackCard: List[Card], defendCard: List[Card]): Boolean = true
    }


    val classOfRobot = classOf[Robot]
    val classOfHuman = classOf[Human]

    val roundEnd: RoundEnd = if (defends.hiddenDiscard.isEmpty && Validate.canBeat(attackedCard, defends.defendCards)) {
      val newAttacker: Player = defends.player match {
        case human: Human =>
          Human(
            human.name,
            List.empty[Card],
            human.points,
            human.tricks ++ attackedCard ++ defends.defendCards,
            human.hiddenTricks,
            true
          )
        case robot: Robot =>
          Robot(
            robot.name,
            List.empty[Card],
            robot.points,
            robot.tricks ++ attackedCard ++ defends.defendCards,
            robot.hiddenTricks
          )
      }
      //
      val newDefender: Player = attackerPlayer match {
        case human: Human => Human(human.name, List.empty[Card], human.points, human.tricks, human.hiddenTricks)
        case robot: Robot => Robot(robot.name, List.empty[Card], robot.points, robot.tricks, robot.hiddenTricks, false)
      }

      RoundEnd(newAttacker, newDefender, upgradedCardDesk)
    } else if (defends.hiddenDiscard.nonEmpty) {
      val newAttacker: Player = attackerPlayer match {
        case human: Human =>
          Human(
            human.name,
            List.empty[Card],
            human.points,
            human.tricks ++ attackedCard,
            human.hiddenTricks ++ defends.hiddenDiscard,
            true
          )
        case robot: Robot =>
          Robot(
            robot.name,
            List.empty[Card],
            robot.points,
            robot.tricks ++ attackedCard,
            robot.hiddenTricks ++ defends.hiddenDiscard
          )
      }

      val newDefender: Player = defends.player match {
        case human: Human => Human(human.name, List.empty[Card], human.points, human.tricks, human.hiddenTricks)
        case robot: Robot => Robot(robot.name, List.empty[Card], robot.points, robot.tricks, robot.hiddenTricks, false)
      }

      RoundEnd(newAttacker, newDefender, upgradedCardDesk)
    } else {
      val newAttacker: Player = attackerPlayer match {
        case human: Human =>
          Human(
            human.name,
            List.empty[Card],
            human.points,
            human.tricks ++ attackedCard,
            human.hiddenTricks ++ defends.defendCards,
            true
          )
        case robot: Robot =>
          Robot(
            robot.name,
            List.empty[Card],
            robot.points,
            robot.tricks ++ attackedCard,
            robot.hiddenTricks ++ defends.defendCards
          )
      }

      val newDefender: Player = defends.player match {
        case human: Human => Human(human.name, List.empty[Card], human.points, human.tricks, human.hiddenTricks)
        case robot: Robot => Robot(robot.name, List.empty[Card], robot.points, robot.tricks, robot.hiddenTricks, false)
      }

      RoundEnd(newAttacker, newDefender, upgradedCardDesk)
    }

    println(s"${roundEnd.attacker.name} point = ${roundEnd.attacker.getTricksPoints}")
    println(s"${roundEnd.defender.name} point = ${roundEnd.defender.getTricksPoints}")

    println(s"${roundEnd.attacker.name} trick card = ${roundEnd.attacker.tricks}")
    println(s"${roundEnd.defender.name} trick card = ${roundEnd.defender.tricks}")
    println("")

    case class Winner(attacker: Player, win: Option[Boolean] = None) {
      def apply(attacker: Player): Winner = attacker match {
        case _: Human =>
          println(s"Our tricks points = ${attacker.trickValue}")
          println("Choose : 1 = Carry on game! 2 = Opening cards!")
          val selectedAction: String = io.StdIn.readLine()

          selectedAction match {
            case "1" => Winner(attacker,win)
            case "2" => if (attacker.allCardsValue >= 30) Winner(attackerPlayer, Some(true)) else Winner(attacker, Some(false))
          }

        case robot: Robot if robot.getTricksPoints >= 30 => Winner(attacker, Some(true))
        case _ => Winner(attacker, win)
      }
    }

    val winTheGame: Winner = Winner(roundEnd.attacker)

    winTheGame.win match {
      case Some(value) if value => roundEnd.attacker match {
        case human: Human =>
          val robot: Robot = roundEnd.defender match {case robot: Robot => robot}
          val newCardDesk: CardDesk       = CardDesk().setUpTrump

          Game().apply(robot, human, newCardDesk)

        case robot: Robot =>
          val human: Human = roundEnd.defender match {case human: Human => human}
          val newCardDesk: CardDesk       = CardDesk().setUpTrump

          Game().apply(robot, human, newCardDesk)
      }
      case Some(value) if !value => roundEnd.attacker match {
        case human: Human =>
          val robot: Robot = defendPlayer match {
            case robot: Robot => robot
          }
          val newCardDesk: CardDesk = CardDesk().setUpTrump

          Game().apply(robot, human, newCardDesk)

        case robot: Robot =>
          val human: Human = roundEnd.defender match {
            case human: Human => human
          }
          val newCardDesk: CardDesk = CardDesk().setUpTrump

          Game().apply(robot, human, newCardDesk)
      }//Todo need write +1 to player ho is win, before need create param in Players
      case _ => this
    }


    def bothOpenCard(attacker: Player, defender: Player): Game = {
      val attackerPoints: Int = attacker.allCardsValue
      val defenderPoints: Int = defender.allCardsValue

      if (attackerPoints > defenderPoints) attacker match {
        case human: Human => Game().apply(Robot(attack = false), Human(name = human.name, attack = true), CardDesk().setUpTrump)
        case _: Robot => Game().apply(Robot(), Human(name = human.name), CardDesk().setUpTrump) //Todo need make each player gameWins: Int = 0
      }
      else if (defenderPoints > attackerPoints) defender match {
        case human: Human => Game().apply(Robot(attack = false), Human(name = human.name, attack = true), CardDesk().setUpTrump)
        case _: Robot => Game().apply(Robot(), Human(name = human.name), CardDesk().setUpTrump) //Todo need make each player gameWins: Int = 0
      }
      else Game().apply(Robot(), Human(human.name), CardDesk().setUpTrump)
    }


    println(s"${roundEnd.attacker} -> attacker")
    println(s"${roundEnd.defender} -> defender")

    if (upgradedCardDesk.cards.size < roundEnd.attacker.needCard * 2) bothOpenCard(roundEnd.attacker, roundEnd.defender)
    else {
      (roundEnd.attacker, roundEnd.defender) match {
        case (human: Human, robot: Robot) => Game().apply(Robot(points = robot.points, attack = false), Human(name = human.name, points = human.points, attack = true), roundEnd.cardDesk)
        case (robot: Robot, human: Human) => Game().apply(Robot(points = robot.points, tricks = robot.tricks), Human(name = human.name, points = human.points, tricks = human.tricks), roundEnd.cardDesk)
      }
    }

  }

}
