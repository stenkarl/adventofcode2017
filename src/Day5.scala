import scala.io.Source

object Day5 {

  def main(args:Array[String]):Unit = {
    println("Advent of Code: Day 5")

    var numbers:List[Int] = Source.fromFile("day5.txt").getLines.map(_.toInt).toList

    var currentIndex = 0
    var numMoves = 0
    println ("Initial: " + numbers)
    while (currentIndex < numbers.size ) {
      val spacesToMove = numbers(currentIndex)
      val amtToAdd = if (spacesToMove >= 3) -1 else 1
      numbers = numbers.updated(currentIndex, spacesToMove + amtToAdd)
      numMoves = numMoves + 1
      println ("Move #" + numMoves + " at " + currentIndex + " we move " + spacesToMove +
        " and add 1 to " + spacesToMove + " to get " + (spacesToMove + 1)
         + " next index " + (currentIndex + spacesToMove) + " " + numbers)
      currentIndex = currentIndex + spacesToMove

    }

    println ("Found the exit of the list after " + numMoves + " moves")

  }


}
