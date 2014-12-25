object Pretty {
  def prettyPrint(game: Game): Unit = {
    (game.currentPlayer :: game.otherPlayers).zipWithIndex foreach {
      case (player, index) ⇒
        print(s"${index + 1}. ")
        prettyPrint(player)
    }

    val burn = (game.burn map (_.value)).sorted mkString ", "
    println(s"Burn: $burn")
  }

  def prettyPrint(player: Player): Unit = {
    val grouped: List[String] = for {
      group ← Card.grouped(player.cards)
    } yield group match {
      case single :: Nil ⇒ single.value.toString
      case group         ⇒ s"(${group map (_.value) mkString ", "})"
    }

    println(s"${player.score} (${player.coins} / ${grouped mkString ", "})")
  }
}
