import scala.util.Try

object Main extends App {
  val DefaultPlayerCount: Int = 6
  val playerCount: Int =
    Try(args.head.toInt).toOption getOrElse DefaultPlayerCount

  val strategies: List[Strategy] = List.fill(playerCount)(Strategy.AlwaysTake)
  val players: List[Player] = strategies map Player.init

  val init: Game = Game.init(players)
  val turns: Stream[Game] = Stream.iterate(init)(Impl.next)

  val finalState: Game =
    (turns dropWhile (!_.isFinished)).head

  println(finalState)
}
