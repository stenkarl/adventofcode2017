import scala.io.Source

object Day9 {

  def main(args:Array[String]):Unit = {
    println("Advent, Day 9")

    val input:String = Source.fromFile("day9.txt").getLines.toList(0)

    println("Raw Input size :" + input.size + " " + input)

    val noNopes = removeNopes(input)
    println("No Nopes size  :" + noNopes.size + " " + noNopes)

    val noGarbage = removeGarbage(noNopes)
    println("No Garbage size: " + noGarbage.size + " " + noGarbage)

    val noComma = noGarbage.replaceAll(",", "")

    println("No Comma size  : " + noComma.size + " " + noComma)

    println("Score = " + score(noComma))
  }

  def removeNopes(input:String):String = {
    var i:Int = 0
    var noNopes = ""
    while (i < input.size) {
      if (input(i) == '!') {
        i = i + 1
      } else {
        noNopes = noNopes + input(i)
      }
      i = i + 1
    }
    noNopes
  }

  def removeGarbage(input: String):String = {
    var i:Int = 0
    var noGarbage = ""
    var collectingGarbage = false
    var numGarbage = 0
    while (i < input.size) {
      if (input(i) == '<' && !collectingGarbage) {
        collectingGarbage = true
      } else if (input(i) == '>') {
        collectingGarbage = false
      } else if (!collectingGarbage) {
        noGarbage = noGarbage + input(i)
      } else {
        numGarbage = numGarbage + 1
      }
      i = i+1
    }

    println("num garbage = " + numGarbage)
    noGarbage
  }

  def score(input:String):Int = {
    var curScore = 0
    var curLevel = 0
    var i:Int = 0
    while (i < input.size) {
      if (input(i) == '{') {
        curLevel = curLevel + 1
      } else if (input(i) == '}') {
        curScore = curScore + curLevel
        curLevel = curLevel - 1
      }
      i = i + 1
    }
    curScore
  }

  def assignLevels(input:String):Int = {
    var curScore = 0
    var curLevel = 0
    var i:Int = 0
    var levelString = ""
    while (i < input.size) {
      if (input(i) == '{') {
        curLevel = curLevel + 1
      } else if (input(i) == '}') {
        curScore = curScore + curLevel
        curLevel = curLevel - 1
      }
      i = i + 1
    }
    curScore
  }

}
