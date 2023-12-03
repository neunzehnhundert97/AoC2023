//> using dep com.lihaoyi::fastparse:3.0.2
import fastparse.Parsed.Failure
import fastparse.Parsed.Success
import scala.io.Source

import fastparse.*, NoWhitespace.*

def part1(lines: List[String]) = {
  val knownContent = Map("red" -> 12, "green" -> 13, "blue" -> 14)

  val games = lines
    .map(line => parse(line, parser(_)))
    .map {
      case Success(value, _) => value
      case _: Failure        => throw Exception("Illegal input")
    }

  val possibleGames = games
    .filterNot { case Game(_, runs) =>
      runs.exists { run =>
        println(run.draws.length)
        run.draws.exists { case Draw(amount, color) =>
          knownContent.get(color).get < amount
        }
      }
    }
    .map(_.index)

  println(possibleGames)

  possibleGames.sum
}

def part2(lines: List[String]) = {
  val games = lines
    .map(line => parse(line, parser(_)))
    .map {
      case Success(value, _) => value
      case _: Failure        => throw Exception("Illegal input")
    }

  games.map { game =>
    val neededCubes = game.runs.foldLeft((0, 0, 0)) {
      case ((red, green, blue), run) =>
        (
          (red :: run.draws.collect { case Draw(amount, "red") => amount }).max,
          (green :: run.draws.collect { case Draw(amount, "green") =>
            amount
          }).max,
          (blue :: run.draws.collect { case Draw(amount, "blue") =>
            amount
          }).max
        )
    }
    neededCubes._1 * neededCubes._2 * neededCubes._3
  }.sum
}

final case class Draw(amount: Int, color: String)
final case class Run(draws: List[Draw])
final case class Game(index: Int, runs: List[Run])

def parser[$: P] = {
  def number[$: P] = P(CharIn("0123456789")).rep(1)
  def title[$: P] = P("Game ") ~ number.!
  def color[$: P] = P(("blue" | "red" | "green"))
  def draw[$: P] =
    (number.! ~ P(" ") ~ color.!).map((num, color) => Draw(num.toInt, color))
  def run[$: P] =
    ((draw ~ P(", ").?).rep(1) ~ P("; ").?).map(draws => Run(draws.toList))

  (title ~ P(": ") ~ run.rep(1) ~ End).map((index, runs) =>
    Game(index.toInt, runs.toList)
  )
}

val exampleData = Source.fromFile("02.example.data").getLines().toList
val realData = Source.fromFile("02.data").getLines().toList

println("Part 1:")
val test1 = part1(exampleData)
assert(test1 == 8, "Part 1 did not pass the test")
println(part1(realData))

println("Part 2:")
val test2 = part2(exampleData)
assert(test2 == 2286, "Part 2 did not pass the test")
println(part2(realData))
