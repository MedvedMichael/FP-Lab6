package solver

import scala.:+
import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParRange

object FunctionSolver extends App {
  def calculateFunction5: PartialFunction[(Double, Double), Double] = {
    case (x, k) if x != 10 =>
      if (x > 10)
        sum(x, 1, 8)
      else
        k * scala.math.pow(x, k)
  }

  def sum(x: Double, start: Int, end: Int): Double = {
    @tailrec
    def sumRecurs(x: Double, start: Int, end: Int, result: Double): Double =
      if (start == end)
        result + x * start
      else
        sumRecurs(x, start + 1, end, result + x * start)

    sumRecurs(x, start, end, 0)
  }


  def toList(range: ParRange, k: Double, numTasks: Int): List[Double] = {
    val partLength = math.floor(range.size.toDouble / numTasks.toDouble).toInt
    var arr = List[(Int, List[Double])]()

    def toListRecurs(i: Int): Unit = {
      if (i < range.size) {
        scalashop.parallel(
          {
            val delta = (i, range.slice(i, i + partLength)
              .map(x => (x.toDouble, k))
              .collect(calculateFunction5)
              .toList)
            arr = delta +: arr
          },
          toListRecurs(i + partLength))
      }
    }

    toListRecurs(0)
    arr.sortWith((a,b) => a._1 < b._1).flatMap {
      case(_, list) => list
    }
  }

  def toList(range: Seq[Int], k: Double): List[Double] =
    range.map(x => (x.toDouble, k)).collect(calculateFunction5).toList


  val list: List[Double] = toList(new ParRange(-250 to 250), 2, 3)
  println(list.zipWithIndex)
  print(toList(-250 to 250, 2).zipWithIndex)


}
