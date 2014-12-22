import shapeless.{ :: ⇒ _, _ }
import scalaz.syntax.id._

trait Choice
case object Take extends Choice
case object Pass extends Choice

trait Strategy {
  def choose(game: Game): Choice
}

object Impl {

  def next(strategy: Strategy)(game: Game): Game =
    game |>
    choose(strategy) |>
    applyChoice(game) |>
    cyclePlayers |>
    saveLastState(game)

  def choose(strategy: Strategy)(game: Game): Choice =
    strategy.choose(game) match {
      case Pass if game.currentPlayer.isEmpty ⇒
        println(s"> strategy chose 'pass' with no coins, forcing 'take': $game")
        Take
      case choice ⇒
        choice
    }

  def applyChoice(game: Game)(choice: Choice): Game = {
    val coinsL = lens[Game] >> 'currentPlayer >> 'coins
    val cardsL = lens[Game] >> 'currentPlayer >> 'cards
    val potL   = lens[Game] >> 'pot
    val deckL  = lens[Game] >> 'deck

    choice match {
      case Take ⇒
        (coinsL ~ potL ~ cardsL ~ deckL).modify(game) {
          case (coins, pot, cards, deck) ⇒
            val currentCard :: rest = deck
            (coins + pot, 0, currentCard :: cards, rest)
        }
      case Pass ⇒
        (coinsL ~ potL).modify(game) {
          case (coins, pot) ⇒
            (coins - 1, pot + 1)
        }
    }
  }

  def cyclePlayers(game: Game): Game = {
    val currentPlayerL = lens[Game] >> 'currentPlayer
    val otherPlayersL  = lens[Game] >> 'otherPlayers

    (currentPlayerL ~ otherPlayersL).modify(game) {
      case (currentPlayer, otherPlayers) ⇒
        val nextPlayer :: rest = (otherPlayers :+ currentPlayer)
        (nextPlayer, rest)
    }
  }

  def saveLastState(last: Game)(game: Game): Game = {
    val lastL = lens[Game] >> 'last

    lastL.set(game)(Option(last))
  }
}
