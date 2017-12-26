import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day16 {

  val buffer = ListBuffer("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
  var cache:Map[String, String] = Map()

  def spin(num:Int):Unit = {
    //println("spin " + num)
    val from = buffer.length - num
    val sublist = buffer.slice(from, buffer.length)
    //println("sublist " + sublist)

    buffer.remove(from, num)

    buffer.prependAll(sublist)
    //println (buffer)
  }

  def exchange(pos1:Int, pos2:Int):Unit = {
    val temp = buffer(pos1)
    buffer(pos1) = buffer(pos2)
    buffer(pos2) = temp
  }

  def partner(a:String, b:String):Unit = {
    exchange(buffer.indexOf(a), buffer.indexOf(b))
  }

  def doMove(str:String):(() => Unit) = {
    str.toList match {
      case 's' :: a :: Nil => { () => spin(a.toString.toInt) }
      case 's' :: a1 :: a2 :: Nil => { () => spin((a1.toString + a2.toString).toInt) }
      case 'x' :: a :: '/' :: b :: Nil => { () => exchange(a.toString.toInt, b.toString.toInt) }
      case 'x' :: a1 :: a2 :: '/' :: b1 :: b2 :: Nil => { () =>
                    exchange((a1.toString + a2.toString).toInt, (b1.toString + b2.toString).toInt) }
      case 'x' :: a1 :: a2 :: '/' :: b :: Nil => { () =>
                    exchange((a1.toString + a2.toString).toInt, b.toString.toInt) }
      case 'x' :: a :: '/' :: b1 :: b2 :: Nil => {() =>
        exchange(a.toString.toInt, (b1.toString + b2.toString).toInt) }
      case 'p' :: a :: '/' :: b :: Nil => { () => partner(a.toString, b.toString) }
      case _ => {() => println("Unknown")}
    }
  }

  var danceNum = 0
  var checkpoint = System.currentTimeMillis()

  def dance(moves:List[String]) = {
    danceNum += 1
    moves.foreach { move =>
      doMove(move).apply()
    }

    if (danceNum % 1000 == 0) {
      println(danceNum + " in " + ((System.currentTimeMillis() - checkpoint)/1000) + "s => " + buffer.mkString(""))
      checkpoint = System.currentTimeMillis()
    }
  }

  def constructMoves(commands: List[String]):List[(() => Unit)] = {
    commands.map( c => doMove(c))
  }


  def cachedDance(moves:List[(() => Unit)]): Unit = {
    danceNum += 1

    if (danceNum % 1000000 == 0) {
      println(danceNum + " in " + ((System.currentTimeMillis() - checkpoint)/1000) + "s => " +
        buffer.mkString(""))
      checkpoint = System.currentTimeMillis()
    }
    val startConfig = buffer.mkString("")
    val inCache = cache.contains(startConfig)
    if (inCache) {
      val cached:String = cache.get(startConfig).get
      for (i <- 0 until cached.size) {
        buffer(i) = cached.charAt(i) + ""
      }
      return
    }

    moves.foreach( m => {
      m.apply()
    })

    cache += (startConfig -> buffer.mkString(""))

  }

  def main(args:Array[String]):Unit = {
    val commands = Source.fromFile("day16.txt").getLines().toList(0).split(',').toList

    val start = System.currentTimeMillis()
    val moves:List[(() => Unit)] = constructMoves(commands)
    println ("constructed " + moves.size + " in " + (System.currentTimeMillis() - start) + "ms")

    for (n <- 0 until 1000000000) cachedDance(moves)
    //for (n <- 0 until 1000000000) dance(commands)

    println (buffer.mkString(""))
  }

}
