
object Day10 {

  val suffix:List[Byte] = List(17, 31, 73, 47, 23)
  val input = "34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167"
  val lengths:List[Byte] = input.getBytes.toList ++ suffix
  val max = 256
  var skip = 0
  var curPos = 0
  var list = Range(0, max).toList

  //val lengths = List(3, 4, 1, 5)
  //val lengths = List(34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167)

  def step(list:List[Int], length: Int, curPos:Int, skipSize:Int): List[Int] = {
    val span = curPos + length
    val wrapped = span - list.size
    println ("Begin step:" + skipSize + ", curPos:" + curPos +
      ", length " + length + ", span:" + span + ", wrapped:" + wrapped + ", " + list)

    val sublist = if (span < list.size) {
      list.slice(curPos, span)
    } else {
      println("slicing " + curPos + " to " + list.size + " with 0 to " + wrapped)
      list.slice(curPos, list.size) ++ list.slice(0, wrapped)
    }
    val reversed = sublist.reverse
    println("Reversing:" + sublist)


    if (span < list.size) {
      //println("Patching: " + reversed + " at " + curPos + " for length " + length)
      list.patch(curPos, reversed, length)
    }
    else {
      val firstLength = list.size - curPos
      val firstHalf = reversed.slice(0, firstLength)
      //println("Wrapped Patching: " + reversed)
      //println("first patch: " + firstHalf + " starting at " + curPos + " for length " + firstLength)

      val firstPatch = list.patch(curPos, firstHalf, firstLength)
      //println("firstPatch: " + firstPatch)

      val secondLength = length - firstLength
      val secondHalf = reversed.slice(firstLength, reversed.size)
      //println("second patch: " + secondHalf + " starting at 0 for length " + secondLength)
      firstPatch.patch(0, secondHalf, secondLength)
    }
  }

  def round(num:Int):Unit = {

    lengths.foreach { len =>
      list = step(list, len, curPos, skip)
      curPos = curPos + len + skip
      println("curPos is now " + curPos + ", len: " + len + " skip " + skip)
      if (curPos >= max) {
        curPos = curPos % max
        println ("curPos has wrapped: " + curPos)
      }
      skip = skip + 1
      println("Round " + num + ", skip:" + skip + ", curPos:" + curPos + ", len:" + len + ", " + list)
    }
    //println("checksum:" + (list(0) * list(1)))

  }

  def xor(sublist:List[Int]): Int = {
    val x = sublist.foldLeft(0)((a, b) => a ^ b)

    println ("xor of " + sublist + " = " + x)

    x
  }

  def main(args:Array[String]):Unit = {
    println("bytes:" + lengths)

    for (i <- 1 to 64) round(i)

    val result:List[Int] = (for (i <- 0 to 15) yield xor(list.slice(i * 16, i * 16 + 16))).toList

    println(result)

    val hex = result.map(_.toHexString)

    println(hex)
  }

}