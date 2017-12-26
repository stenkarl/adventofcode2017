import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day14 {

  def binaryKnotHash(input:String): String = {
    var hex:String = Day10.knotHash(input)
    while (hex.length < 32) hex = "0" + hex
    val binary = hex.map { h =>
      var str = BigInt(h.toString, 16).toString(2)
      while (str.length < 4) str = "0" + str
      str
    }.mkString("")

    println(binary.replaceAll("0", " "))

    binary
  }

  def binaryGrid(input:String):List[String] = {
    (for (n <- 0 until 128) yield binaryKnotHash(input + "-" + n)).toList
  }

  case class Point(x:Int, y:Int)

  def merge(groups:ListBuffer[Set[Point]]):ListBuffer[Set[Point]] = {
    var anyMerged = false
    var i = 0
    while (!anyMerged && i < groups.size) {
      val curSet = groups(i)

      val neighbors = findNeighbors(curSet, groups, i)
      if (neighbors.nonEmpty) {
        anyMerged = true
        //println("Merging " + curSet + " with " + neighbors)
        groups -= curSet
        groups += curSet ++ neighbors
      }
      i = i + 1
    }
    groups
  }

  /**
    * Returned list is the concatenated list of all neighbors of the given point list.
    * The neighbor lists are removed from the group list. An empty returned list means
    * no neighbors found for that list of points in the group.
    */
  def findNeighbors(points: Set[Point], groups: ListBuffer[Set[Point]], i: Int):Set[Point] = {
    var neighborGroups:ListBuffer[Set[Point]] = ListBuffer()
    points.foreach { point =>
      groups.foreach { group =>
        if (groupContainsNeighbor(group, point)) {
          //println ("adding " + group + " to " + neighborGroups)
          neighborGroups += group
        }
      }
    }
    groups --= neighborGroups
    neighborGroups.flatten.toSet
  }

  def groupContainsNeighbor(group:Set[Point], p:Point): Boolean = {
    if (group.contains(p)) {
      return false
    } // don't add the same list as a neighbor
    else if (group.contains(Point(p.x+1, p.y)) ||
              group.contains(Point(p.x-1, p.y)) ||
              group.contains(Point(p.x, p.y+1)) ||
              group.contains(Point(p.x, p.y-1))) {
      return true
    }
    false
  }

  def main(args:Array[String]):Unit = {

    val grid = binaryGrid("wenycdww")

    //val grid = binaryGrid("flqrgnkx")

    //val grid:List[String] = Source.fromFile("flqrgnkx.txt").getLines.toList

    var groups:ListBuffer[Set[Point]] = (for (y <- grid.indices;
                                              x <- grid.indices
                                              if grid(y)(x) == '1')
                                        yield Set(Point(x, y))).to[ListBuffer]

    println("Before merge: " + groups.size + " " + groups)
    var prevSize = groups.size
    do {
      prevSize = groups.size
      groups = merge(groups)
      println("After merge: " + groups.size + " " + groups)
    } while (prevSize != groups.size)
    println("After merge: " + groups.size + " " + groups)

    //val num = grid.map { s =>
    //  s.map(ch => if (ch == '1') 1 else 0).sum
    //}

    //println(num.sum)
    //println(grid.map(s => s.map(ch => if (ch == '1') 1 else 0)).sum)
  }

}
