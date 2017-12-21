
object Day10 {

  //val lengths = List(3, 4, 1, 5)
  val lengths = List(34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167)

  def step(list:List[Int], length: Int, curPos:Int, skipSize:Int): List[Int] = {
    println ("Begin step:" + skipSize + ", curPos:" + curPos +
              ", length " + length + " " + list)
    val span = curPos + length
    val wrapped = span - list.size
    val sublist = if (span < list.size) {
      list.slice(curPos, span)
    } else {

      list.slice(curPos, list.size) ++ list.slice(0, wrapped)
    }
    val reversed = sublist.reverse
    println("Reversing:" + sublist)


    if (span < list.size) {
      println("Patching: " + reversed + " at " + curPos + " for length " + length)
      list.patch(curPos, reversed, length)
    }
    else {
      val firstLength = list.size - curPos
      val firstHalf = reversed.slice(0, firstLength)
      println("Wrapped Patching: " + reversed)
      println("first patch: " + firstHalf + " starting at " + curPos + " for length " + firstLength)

      val firstPatch = list.patch(curPos, firstHalf, firstLength)
      println("firstPatch: " + firstPatch)

      val secondLength = length - firstLength
      val secondHalf = reversed.slice(firstLength, reversed.size)
      println("second patch: " + secondHalf + " starting at 0 for length " + secondLength)
      firstPatch.patch(0, secondHalf, secondLength)
    }
  }

  def main(args:Array[String]):Unit = {
    var skip = 0
    var curPos = 0
    val max = 256
    var list = Range(0, max).toList
    lengths.foreach { len =>
      list = step(list, len, curPos, skip)
      curPos = curPos + len + skip
      if (curPos >= max) {
        curPos = curPos - max
        println ("curPos has wrapped: " + curPos)
      }
      skip = skip + 1
      println("skip:" + skip + ", curPos:" + curPos + ", " + list)
    }
    println("checksum:" + (list(0) * list(1)))
  }

}
