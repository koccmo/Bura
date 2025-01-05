package com.bura.states

import com.bura.domain._
import com.bura.services.{Attack, PlayerDefend, RobDefend, Win}

case class Game() {
  def apply(
    robot: Robot,
    human: Human,
    cardDesk: CardDesk
  ): Game = {

    val humanCards: List[Card]     = cardDesk.get(human.needCard)
    val robotCards: List[Card]     = cardDesk.get(robot.needCard)
    val dealtHuman: Human          = human.setHand(humanCards)
    val dealtRobot: Robot          = robot.setHand(robotCards)
    val upgradedCardDesk: CardDesk = cardDesk.upgrade(humanCards ++ robotCards)
    //Todo need fix card dealing ,when get for any first player card need same time do cardDesk upgrade


    val attackedCard: List[Card] = if (dealtRobot.attack) {
      val cards: List[Card] = Attack.attack(dealtRobot, upgradedCardDesk)
      println(s"Robot attack cards -> ${cards.mkString(", ")}")

      cards
    } else {
      val cards: List[Card] = Attack.attack(dealtHuman, upgradedCardDesk)
      println(s"${dealtHuman.name} attack cards -> $cards") // println need println after Trump!!!!

      cards
    }

    val attackerPlayer: Player = if (dealtRobot.attack) dealtRobot else dealtHuman
    val defendPlayer: Player   = if (dealtHuman.attack) dealtRobot else dealtHuman
    println(s"Attack player -> $attackerPlayer")
    println(s"DefendPlayer -> $defendPlayer")

//    val defends: BeatDiscard = if (!dealtRobot.attack) {
//      RobDefend.defend(attackedCard, dealtRobot, upgradedCardDesk)
//    } else PlayerDefend.defend(attackedCard, dealtHuman, upgradedCardDesk)

    val round = Round()
    val roundEnd: RoundEnd = round(attackerPlayer,defendPlayer, upgradedCardDesk)

    val classOfRobot = classOf[Robot]
    val classOfHuman = classOf[Human]



    println(s"${roundEnd.attacker.name} point = ${roundEnd.attacker.getTricksPoints}")
    println(s"${roundEnd.defender.name} point = ${roundEnd.defender.getTricksPoints}")

    println(s"${roundEnd.attacker.name} trick card = ${roundEnd.attacker.tricks}")
    println(s"${roundEnd.defender.name} trick card = ${roundEnd.defender.tricks}")
    println("")

//    case class Winner(attacker: Player, win: Option[Boolean] = None) {
//      def apply(attacker: Player): Winner = attacker match {
//        case _: Human =>
//          println(s"Our tricks points = ${attacker.trickValue}")
//          println("Choose : 1 = Carry on game! 2 = Opening cards!")
//          val selectedAction: String = io.StdIn.readLine()
//
//          selectedAction match {
//            case "1" => Winner(attacker, win)
//            case "2" =>
//              if (attacker.allCardsValue >= 30) Winner(attackerPlayer, Some(true))
//              else Winner(attacker, Some(false))
//          }
//
//        case robot: Robot if robot.getTricksPoints >= 30 => Winner(attacker, Some(true))
//        case _                                           => Winner(attacker, win)
//      }
//    }

    val win = Win()
    val winTheGame: Winner = win(roundEnd.attacker)

    winTheGame.win match {
      case Some(value) if value  =>
        roundEnd.attacker match {
          case human: Human =>
            val robot: Robot          = roundEnd.defender match { case robot: Robot => robot }
            val newCardDesk: CardDesk = CardDesk().setUpTrump

            Game().apply(robot, human, newCardDesk)

          case robot: Robot =>
            val human: Human          = roundEnd.defender match { case human: Human => human }
            val newCardDesk: CardDesk = CardDesk().setUpTrump

            Game().apply(robot, human, newCardDesk)
        }
      case Some(value) if !value =>
        roundEnd.attacker match {
          case human: Human =>
            val robot: Robot          = defendPlayer match {
              case robot: Robot => robot
            }
            val newCardDesk: CardDesk = CardDesk().setUpTrump

            Game().apply(robot, human, newCardDesk)

          case robot: Robot =>
            val human: Human          = roundEnd.defender match {
              case human: Human => human
            }
            val newCardDesk: CardDesk = CardDesk().setUpTrump

            Game().apply(robot, human, newCardDesk)
        } //need write +1 to player ho is win, before need create param in Players
      case _                     => this
    }

    def bothOpenCard(attacker: Player, defender: Player): Game = {
      val attackerPoints: Int = attacker.allCardsValue
      val defenderPoints: Int = defender.allCardsValue

      if (attackerPoints > defenderPoints) attacker match {
        case human: Human =>
          Game().apply(Robot(attack = false), Human(name = human.name, attack = true), CardDesk().setUpTrump)
        case _: Robot     =>
          Game()
            .apply(Robot(), Human(name = human.name), CardDesk().setUpTrump) //need make each player gameWins: Int = 0
      }
      else if (defenderPoints > attackerPoints) defender match {
        case human: Human =>
          Game().apply(Robot(attack = false), Human(name = human.name, attack = true), CardDesk().setUpTrump)
        case _: Robot     =>
          Game()
            .apply(Robot(), Human(name = human.name), CardDesk().setUpTrump) //need make each player gameWins: Int = 0
      }
      else Game().apply(Robot(), Human(human.name), CardDesk().setUpTrump)
    }

    println(s"${roundEnd.attacker} -> attacker")
    println(s"${roundEnd.defender} -> defender")

    if (upgradedCardDesk.cards.size < roundEnd.attacker.needCard * 2) bothOpenCard(roundEnd.attacker, roundEnd.defender)
    else {
      (roundEnd.attacker, roundEnd.defender) match {
        case (human: Human, robot: Robot) =>
          Game().apply(
            Robot(points = robot.points, attack = false),
            Human(name = human.name, points = human.points, attack = true),
            roundEnd.cardDesk
          )
        case (robot: Robot, human: Human) =>
          Game().apply(
            Robot(points = robot.points, tricks = robot.tricks),
            Human(name = human.name, points = human.points, tricks = human.tricks),
            roundEnd.cardDesk
          )
      }
    }

  }

}
