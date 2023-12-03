val debug = false
val filename = if (debug) "01.example.data" else "01.data"

val numberMap = Map(
    "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, "nine" -> 9
)


@main
def main = {
  println("Part 1")
  println(part1(parse()))
  println("Part 2")
  println(part2(parse()))
}

def part1(input: Iterator[String]) =
    input.filter(_.nonEmpty).map(line => 
        (line.find(_.isDigit), line.findLast(_.isDigit)) match
            case (Some(first), Some(last)) =>
                val string = s"$first$last"
                // println(string)
                string.toInt
            case a => throw new Exception(s"Illegal input: $a in '$line'")
        ).sum

def part2(input: Iterator[String]) = {
    input.filter(_.nonEmpty).map{line =>
        // Find numbers
        val numbers = (0 until line.length).map(index => line.substring(index)).map{
            case s if s.head.isDigit => Some(s.head.toString.toInt)
            case s => numberMap.find((w, _) => s.startsWith(w)).map(_._2)
        }.flatten
        println(numbers)
        val string = s"${numbers.head}${numbers.last}"
        println(string)
        string.toInt
    }.sum
}

def parse(): Iterator[String] =
  scala.io.Source.fromFile(filename).getLines()
