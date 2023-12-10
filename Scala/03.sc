import scala.util.matching.Regex.Match
import scala.io.Source
final case class Symbol(kind: Char, position: (Int, Int))
final case class Number(value: Int, startPos: (Int, Int), length: Int)

def parse(input: List[String]) = {
  val regex = raw"\d+|[^.]".r

  val tokens = input.zipWithIndex.flatMap { (line, index) =>
    regex
      .findAllMatchIn(line)
      .map {
        case m if m.matched.head.isDigit =>
          Number(m.matched.toInt, (m.start, index), m.end - m.start)
        case m =>
          Symbol(m.matched.head, (m.start, index))
      }
      .toList
  }
  val numbers = tokens.collect { case n: Number =>
    n
  }
  val symbols = tokens.collect { case n: Symbol =>
    n.position -> n.kind
  }.toMap
  (numbers, symbols)
}

def part1(input: List[String]) = {
  val (numbers, symbols) = parse(input)
  val partNumbers = numbers.filter { num =>
    val (x, y) = num.startPos
    val adjecents =
      (x - 1).to(x + num.length).flatMap { xx =>
        ((y - 1).to(y + 1)).map(yy => (xx, yy))
      }

    adjecents.exists(pos => symbols.keySet.contains(pos))
  }
  partNumbers.map(_.value).sum
}

def part2(input: List[String]) = {
  val (numbers, symbols) = parse(input)
  val gears = symbols.toList.filter(_._2 == '*').map(_._1).toSet
  val adjecentGears = numbers
    .flatMap { num =>
      val (x, y) = num.startPos
      val adjecents =
        (x - 1).to(x + num.length).flatMap { xx =>
          ((y - 1).to(y + 1)).map(yy => (xx, yy))
        }
      adjecents
        .filter(gears.contains)
        .map(g => g -> num.value)
    }
    .sortBy(_._1)

  adjecentGears
    .groupMap(_._1)(_._2)
    .filter(_._2.length == 2)
    .mapValues(_.product)
    .map(_._2)
    .sum
}

val exampleData = Source.fromFile("03.example.data").getLines().toList
val realData = Source.fromFile("03.data").getLines().toList

println("Part 1:")
val test1 = part1(exampleData)
if (test1 != 4361) {
  println("Part 1 did not pass the test")
  sys.exit()
}
println(part1(realData))

println("Part 2:")
val test2 = part2(exampleData)
if (test2 != 467835) {
  println(s"Part 2 did not pass the test, it was ${test2}")
  sys.exit()
}
println(part2(realData))
