import scala.io.Source

object Day8 {

  trait BinOp { def apply(original:Int, amt:Int):Int }

  class Inc extends BinOp {
    def apply(original:Int, amt:Int):Int = original + amt
  }

  class Dec extends BinOp {
    def apply(original:Int, amt:Int):Int = original - amt
  }

  trait Condition { def apply(reg:String, amt:Int):Boolean }

  class Equals extends Condition {
    def apply(reg:String, amt:Int):Boolean = { registers(reg) == amt}
  }

  class NotEquals extends Condition {
    def apply(reg:String, amt:Int):Boolean = { registers(reg) != amt}
  }

  class GreaterThan extends Condition {
    def apply(reg:String, amt:Int):Boolean = { registers(reg) > amt}
  }

  class LessThan extends Condition {
    def apply(reg:String, amt:Int):Boolean = { registers(reg) < amt}
  }

  class GreaterThanOrEqual extends Condition {
    def apply(reg:String, amt:Int):Boolean = { registers(reg) >= amt}
  }

  class LesserThanOrEqual extends Condition {
    def apply(reg:String, amt:Int):Boolean = { registers(reg) <= amt}
  }

  var registers:Map[String, Int] = Map()

  val operations:Map[String, BinOp] = Map("inc" -> new Inc(), "dec" -> new Dec())
  val conditions:Map[String, Condition] = Map("==" -> new Equals(),
                    "!=" -> new NotEquals(), ">" -> new GreaterThan(),
            "<" -> new LessThan(), ">=" -> new GreaterThanOrEqual(),
            "<=" -> new LesserThanOrEqual())

  var highest = 0

  def main(args:Array[String]):Unit = {
    val lines:List[String] = Source.fromFile("day8.txt").getLines.toList

    lines.foreach { line => parse(line.split(" ").toList)}

    println(registers)
    println(registers.values.toList.max + ", highest val " + highest)
  }

  def parse(tokens:List[String]):Unit = {
    println(tokens)
    tokens match {
      case reg :: op :: amt :: "if" :: condReg :: condition :: condAmt :: Nil =>
        execute(reg, operations(op), amt.toInt, condReg, conditions(condition), condAmt.toInt)
      case _ => println("Unknown match")
    }
  }

  def execute(reg:String, op:BinOp, amt:Int, condReg:String,
                              condition:Condition, condAmt:Int):Unit = {

    addRegister(reg)
    addRegister(condReg)
    var newVal:Int = registers(reg)

    if (condition.apply(condReg, condAmt)) {
      println(condReg + " " + condition + " " + condAmt)
      newVal = op.apply(registers(reg), amt)
      if (newVal > highest) highest = newVal
    }

    registers = registers.updated(reg, newVal)
  }

  def addRegister(reg:String) = {
    if (!registers.contains(reg)) {
      println("Adding Register " + reg)
      registers = registers + (reg -> 0)
    }
  }

}
