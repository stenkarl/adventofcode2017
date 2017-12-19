import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day7 {

  case class Tower(name:String, weight:Int, children:List[Tower]) {
    def totalWeight:Int = weight + children.map(_.totalWeight).sum
  }

  case class TowerBuilder(name:String, weight:Int, children:List[String])


  var towerMap:Map[String,Tower] = Map()
  var towerBuilderList:ListBuffer[TowerBuilder] = ListBuffer.empty[TowerBuilder]

  def main(args:Array[String]):Unit = {
    val lines:List[String] = Source.fromFile("day7.txt").getLines.toList

    val namesAndWeights = lines.map { line => line.split("\\)")(0).split(" \\(")}

    //val towers = namesAndWeights.map(str => Tower(str(0), str(1).toInt, Nil))

    lines.foreach { str =>
      val split = str.split("\\) -> ")
      val nameWeight = split(0).split(" \\(")
      if (split.size == 1) {
        towerMap = towerMap + (nameWeight(0) -> Tower(nameWeight(0), nameWeight(1).substring(0, nameWeight(1).size-1).toInt, Nil))
      } else {
        towerBuilderList += TowerBuilder(nameWeight(0), nameWeight(1).toInt, split(1).split(", ").toList)
      }
    }
    while (!towerBuilderList.isEmpty) {
      buildTowerMap()
    }
    println(towerMap)

    findImbalance(towerMap("vfjnsd"))

    //findBalance(towerMap("bpvhwhh"), 1)

    //findParent(towerMap("jowbm").name)
    //towerMap.foreach { (entry) =>
    //  println("k:" + entry._1 + ", v:" + entry._2 + ", parent:" + findParent(entry._1))

    //}
  }

  def findImbalance(tower:Tower): Unit = {
    tower.children.foreach { t => println(t.name + " weight " + t.weight + " Total weight " + t.totalWeight)}
  }

  var weightMap:Map[Int, Int] = Map()

  def findBalance(tower:Tower, level:Int): Unit = {
    //println("Level " + level + ", weight of " + tower.name + " is " + tower.totalWeight)
    if (!weightMap.contains(level)) {
      weightMap = weightMap + (level -> tower.totalWeight)
    } else {
      if (tower.totalWeight != weightMap(level)) {
        println("Tower " + tower + " has weight mismatch. Should be " +
          weightMap(level) + " but is " + tower.totalWeight + " at level " + level)
      }
    }
    tower.children.foreach { findBalance(_, level+1)}
  }

  def buildTowerMap() = {
    var toBeRemoved:ListBuffer[TowerBuilder] = ListBuffer.empty[TowerBuilder]
    towerBuilderList.foreach { builder =>
      if (builder.children.forall { child => towerMap.contains(child)}) {
        toBeRemoved += builder
        val children = builder.children.map( c => towerMap(c))
        towerMap = towerMap + (builder.name -> Tower(builder.name, builder.weight, children))
      }
    }

    toBeRemoved.foreach { towerBuilderList -= _ }
  }

  def findParent(name:String): String = {
    towerMap.foreach { (entry) =>
      val t = entry._2
      //if (!t.children.isEmpty) println(t.children.size + " " + t)
      if (t.children.contains(name)) {
        println("Found parent for " + name)
        return entry._1
      }
    }
    println("Unable to find parent for " + name)
    null
  }

}
