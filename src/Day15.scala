
object Day15 {


  case class Generator(factor:BigInt, seed:BigInt, multiple:BigInt) {

    val divBy = BigInt(2147483647)
    var current:BigInt = seed

    var count = 0

    def next():String = {
      current = current * factor % divBy

      if (current % multiple != 0) {
        next()
      }
      count += 1
      if (count % 100000 == 0) println("count:" + count)


      val bin = current.toString(2)

      return if (bin.length > 15) {
        bin.substring(bin.length - 16)
      } else {
        bin
      }

    }
  }

  def main(args:Array[String]):Unit = {
    //val a = Generator(BigInt(16807), BigInt(65), BigInt(4))
    //val b = Generator(BigInt(48271), BigInt(8921), BigInt(8))

    val a = Generator(BigInt(16807), BigInt(722), BigInt(4))
    val b = Generator(BigInt(48271), BigInt(354), BigInt(8))

    //val firstFive = for (n <- 0 until 5) yield (a.next(), b.next())

    val matches = for (n <- 0 until 5000000; if a.next().equals(b.next())) yield 1

    println("matches size " + matches.size)
  }

}
