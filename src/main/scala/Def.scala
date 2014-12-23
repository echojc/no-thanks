import scala.util.Random

trait Choice
case object Take extends Choice
case object Pass extends Choice

object Strategy {
  object AlwaysTake extends Strategy {
    def choose(game: Game): Choice = Take
  }
  object AlwaysPass extends Strategy {
    def choose(game: Game): Choice = Pass
  }
}

trait Strategy {
  def choose(game: Game): Choice
}

object Card {
  val Reference: List[Card] =
    ((3 to 35) map Card.apply).toList
  def randomized: List[Card] =
    Random.shuffle(Reference)
}
case class Card(
  value: Int
)

object Player {
  val DefaultCoins: Int = 7
  def init(strategy: Strategy): Player = Player(DefaultCoins, Nil, strategy)
}
case class Player(
  coins: Int,
  cards: List[Card],
  strategy: Strategy
) {
  def isEmpty: Boolean = coins == 0
}

object Game {
  val DefaultBurnCount: Int = 7
  def init(players: List[Player]): Game = {
    val firstPlayer :: otherPlayers = players
    val (burn, deck) = Card.randomized splitAt DefaultBurnCount
    Game(firstPlayer, otherPlayers, pot = 0, deck, burn)
  }
}
case class Game(
  currentPlayer: Player,
  otherPlayers: List[Player],
  pot: Int,
  deck: List[Card],
  burn: List[Card]
) {
  def currentCard: Card = deck.head
  def isFinished: Boolean = deck.isEmpty
}
