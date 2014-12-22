import scala.util.Random

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
  def init: Player = Player(DefaultCoins, Nil)
}
case class Player(
  coins: Int,
  cards: List[Card]
) {
  def isEmpty: Boolean = coins == 0
}

object Game {
  def init(playerCount: Int): Game = {
    val firstPlayer :: otherPlayers = List.fill(playerCount)(Player.init)
    val (burn, deck) = Card.randomized splitAt 7
    Game(firstPlayer, otherPlayers, pot = 0, deck, burn, None)
  }
}
case class Game(
  currentPlayer: Player,
  otherPlayers: List[Player],
  pot: Int,
  deck: List[Card],
  burn: List[Card],
  last: Option[Game]
) {
  def currentCard = deck.head
}
