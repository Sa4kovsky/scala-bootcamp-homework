import Main.Error.{IncorrectRank, IncorrectSuit, InvalidBoardSize, InvalidHandSize}

import scala.collection.SortedSet

object Main {

  sealed trait Suit
  object Suit {
    final case object Club extends Suit
    final case object Diamond extends Suit
    final case object Heart extends Suit
    final case object Spade extends Suit

    def convertString(value: String): Either[ErrorMessage, Suit] = {
      value match {
        case "c" => Right(Club)
        case "d" => Right(Diamond)
        case "h" => Right(Heart)
        case "s" => Right(Spade)
        case _   => Left(IncorrectSuit)
      }
    }
  }

  sealed trait Rank
  object Rank {
    final case object Two extends Rank
    final case object Three extends Rank
    final case object Four extends Rank
    final case object Five extends Rank
    final case object Six extends Rank
    final case object Seven extends Rank
    final case object Eight extends Rank
    final case object Nine extends Rank
    final case object Ten extends Rank
    final case object Jack extends Rank
    final case object Queen extends Rank
    final case object King extends Rank
    final case object Ace extends Rank

    def convertString(value: String): Either[ErrorMessage, Rank] = {
      value match {
        case "2" => Right(Two)
        case "3" => Right(Three)
        case "4" => Right(Four)
        case "5" => Right(Five)
        case "6" => Right(Six)
        case "7" => Right(Seven)
        case "8" => Right(Eight)
        case "9" => Right(Nine)
        case "T" => Right(Ten)
        case "J" => Right(Jack)
        case "Q" => Right(Queen)
        case "K" => Right(King)
        case "A" => Right(Ace)
        case _   => Left(IncorrectRank)
      }
    }
  }

  sealed trait PokerCombination
  object PokerCombination {
    final object RoyalFlush extends PokerCombination
    final object StraightFlush extends PokerCombination
    final object FourOfAKind extends PokerCombination
    final object FullHouse extends PokerCombination
    final object Flush extends PokerCombination
    final object Straight extends PokerCombination
    final object ThreeOfAKind extends PokerCombination
    final object TwoPair extends PokerCombination
    final object Pair extends PokerCombination
    final object HighCard extends PokerCombination
  }

  sealed case class Card(rank: Rank, suit: Suit)

  sealed case class Board(card: List[Card]) {
    def apply(cards: List[Card]): Either[ErrorMessage, Board] = {
      if (cards.length == 5) Right(Board(cards))
      else Left(InvalidBoardSize)
    }
  }

  sealed trait Hand
  object Hand {
    final case class OmahaHand(cards: (Card, Card, Card, Card) ) extends Hand
    final case class TexasHand(cards: (Card, Card) ) extends Hand
  }

  sealed abstract class ErrorMessage(details: String) { def message: String = s"Error: $details" }
  object Error {
    case object IncorrectSuit extends ErrorMessage("Incorrect suit")
    case object IncorrectRank extends ErrorMessage("Incorrect rank")
    case object InvalidBoardSize extends ErrorMessage("Invalid board size")
    case object InvalidHandSize extends ErrorMessage("Invalid hand size")
  }

  final case class TestCase(board: Board, hands: List[Hand])

  final case class TestResult(board: Board, hands: SortedSet[Hand])

}

