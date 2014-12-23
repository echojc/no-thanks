import shapeless.{ :: ⇒ _, _ }
import scalaz.syntax.id._

object Impl {

  def next(game: Game): Game =
    game |>
    choose(game.currentPlayer.strategy) |>
    applyChoice(game) |>
    cyclePlayers

  def choose(strategy: Strategy)(game: Game): Choice =
    strategy.choose(game) match {
      case Pass if game.currentPlayer.isEmpty ⇒
        println(s"> strategy chose 'pass' with no coins, forcing 'take': $game")
        Take
      case choice ⇒
        choice
    }

  lazy val coinsL = lens[Game] >> 'currentPlayer >> 'coins
  lazy val cardsL = lens[Game] >> 'currentPlayer >> 'cards
  lazy val potL   = lens[Game] >> 'pot
  lazy val deckL  = lens[Game] >> 'deck
  def applyChoice(game: Game)(choice: Choice): Game =
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

  lazy val currentPlayerL = lens[Game] >> 'currentPlayer
  lazy val otherPlayersL  = lens[Game] >> 'otherPlayers
  def cyclePlayers(game: Game): Game =
    (currentPlayerL ~ otherPlayersL).modify(game) {
      case (currentPlayer, otherPlayers) ⇒
        val nextPlayer :: rest = (otherPlayers :+ currentPlayer)
        (nextPlayer, rest)
    }
}
