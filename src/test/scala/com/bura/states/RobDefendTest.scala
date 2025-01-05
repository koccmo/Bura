package com.bura.states

import com.bura.domain.Rank._
import com.bura.domain.Suit.{Clubs, Diamonds, Hearts, Spades}
import com.bura.domain.{Card, CardDesk, Robot}
import com.bura.services.RobDefend
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RobDefendTest extends AnyFunSuite with Matchers {

  test("Robot defend") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Seven, Diamonds), Card(Ten, Clubs), Card(Nine, Clubs))
    val attackCards = List(Card(Eight, Clubs), Card(King, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Nine, Clubs), Card(Ten , Clubs))
  }

  test ("No Trumps, Attack - One Card, Defender Beat ") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Seven, Diamonds), Card(Jack, Clubs), Card(Nine, Spades))
    val attackCards = List(Card(Eight, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Jack, Clubs))
  }

  test("No Trumps, Attack - One Card, Defender HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Seven, Diamonds), Card(Six, Clubs), Card(Nine, Spades))
    val attackCards = List(Card(Eight, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Six, Clubs))
  }

  test("No Trumps, Attack - One Card, Defender has Ten, Ace same suit Beat") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ten, Clubs), Card(Ace, Clubs), Card(Nine, Spades))
    val attackCards = List(Card(Eight, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Ace, Clubs))
  }

  test("No Trumps, Attack - One Card, Defender has Six, Ten same suit Beat") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ten, Clubs), Card(Six, Clubs), Card(Nine, Spades))
    val attackCards = List(Card(Eight, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Ten, Clubs))
  }

  test("No Trumps, Attack - Two Cards, Defender Beat ") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(King, Clubs), Card(Jack, Diamonds), Card(Nine, Clubs))
    val attackCards = List(Card(Eight, Clubs), Card(Jack, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Nine, Clubs), Card(King, Clubs))
  }

  test("No Trumps, Attack - Two Cards, Defender HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Seven, Diamonds), Card(Six, Clubs), Card(Nine, Spades))
    val attackCards = List(Card(Eight, Clubs), Card(Ten, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Six, Clubs), Card(Seven, Diamonds))
  }

  test("No Trump, Attack - Three Cards, Defender Beat") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Nine, Clubs), Card(Seven, Clubs), Card(Ace, Clubs))
    val attackCards = List(Card(Eight, Clubs), Card(Ten, Clubs), Card(Six, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Seven, Clubs), Card(Nine, Clubs), Card(Ace, Clubs))
  }

  test("No Trump, Attack - Three Cards, Defender HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Nine, Clubs), Card(Seven, Clubs), Card(King, Clubs))
    val attackCards = List(Card(Eight, Clubs), Card(Ten, Clubs), Card(Six, Clubs))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Seven, Clubs), Card(Nine, Clubs), Card(King, Clubs))
  }

  test("Attack - Seven trump, Defender Nine trump, Ten Seven not HiddenDiscard") {
    val trumpCard = Card(King, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ten, Diamonds), Card(Seven, Clubs), Card(Nine, Hearts))
    val attackCards = List(Card(Seven, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Seven, Clubs))
  }

  test("Attack - Ten trump, Defender Six, Ace Trump Beat") {
    val trumpCard = Card(King, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Six, Hearts), Card(Seven, Clubs), Card(Ace, Hearts))
    val attackCards = List(Card(Ten, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Ace, Hearts))
  }

  test("Attack - King trump, Defender Six, Ace Trump HiddenDiscard") {
    val trumpCard = Card(King, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Six, Hearts), Card(Seven, Clubs), Card(Ace, Hearts))
    val attackCards = List(Card(King, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Seven, Clubs))
  }

  test("Attack - King trump, Defender No Trump, But has 2 similar suit cards HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Six, Diamonds), Card(Seven, Clubs), Card(Ace, Clubs))
    val attackCards = List(Card(King, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Six, Diamonds))
  }

  test("Attack - Six, King trump, Defender No trump 3 different suits HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ace, Diamonds), Card(Seven, Clubs), Card(Ten, Spades))
    val attackCards = List(Card(King, Hearts), Card(Six, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Seven, Clubs), Card(Ten, Spades))
  }

  test("Attack - Six, King trump, Defender Ten trump -> Nine, King not trump HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ace, Diamonds), Card(Nine, Clubs), Card(Ten, Hearts))
    val attackCards = List(Card(King, Hearts), Card(Six, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Ace, Diamonds), Card(Nine, Clubs))
  }

  test("Attack - Six, Ace trump, Defender Ten, Nine trump -> King not trump HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ace, Diamonds), Card(Nine, Hearts), Card(Ten, Hearts))
    val attackCards = List(Card(Ace, Hearts), Card(Six, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Ace, Diamonds), Card(Nine, Hearts))
  }

  test("Attack - Six, Ace trump, Defender Seven, Nine trump -> King not trump HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ace, Diamonds), Card(Nine, Hearts), Card(Seven, Hearts))
    val attackCards = List(Card(Ace, Hearts), Card(Six, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Ace, Diamonds), Card(Seven, Hearts))
  }

  test("Attack - Six, Ten trump, Defender Ace, Nine trump -> King not trump Beat") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ace, Diamonds), Card(Nine, Hearts), Card(Ace, Hearts))
    val attackCards = List(Card(Ten, Hearts), Card(Six, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Nine, Hearts), Card(Ace, Hearts))
  }

  test("Attack - Six, Jack, Ten trump, Defender Nine, Ace trump King not HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Ace, Diamonds), Card(Nine, Hearts), Card(Ace, Hearts))
    val attackCards = List(Card(Ten, Hearts), Card(Six, Hearts), Card(Jack, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Nine, Hearts), Card(Ace, Diamonds), Card(Ace, Hearts))
  }

  test("Attack - Seven, Jack, Ten trump, Defender Nine, Ace, Six -> HiddenDiscard") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Six, Hearts), Card(Nine, Hearts), Card(Ace, Hearts))
    val attackCards = List(Card(Ten, Hearts), Card(Seven, Hearts), Card(Jack, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).hiddenDiscard

    defendCard shouldBe List(Card(Six, Hearts), Card(Nine, Hearts), Card(Ace, Hearts))
  }

  test("Attack - Seven, Jack, Ten trump, Defender Nine, Ace, Queen -> Beat") {
    val trumpCard = Card(Ten, Hearts)
    val cardDesk = new CardDesk(trump = Some(trumpCard))
    val robotCards = List(Card(Queen, Hearts), Card(Nine, Hearts), Card(Ace, Hearts))
    val attackCards = List(Card(Ten, Hearts), Card(Seven, Hearts), Card(Jack, Hearts))
    val robot = Robot().setHand(robotCards)
    val defendCard = RobDefend.defend(attackCards, robot, cardDesk).defendCards

    defendCard shouldBe List(Card(Nine, Hearts), Card(Queen, Hearts), Card(Ace, Hearts))
  }
}
