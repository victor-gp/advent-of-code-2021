// https://adventofcode.com/2021/day/7

import scala.io.Source

@main def day07(part: Int): Unit = {
  val source = Source.fromFile("../../input/day07-in.txt")
  val input = source.mkString
  source.close

  val crabPositions = input.trim.split(",").map(_.toInt).toList

  if part != 1 && part != 2 then bad_argv1
  val positionCosts = part match
    case 1 => part1(crabPositions)
    case 2 => part2(crabPositions)

  val (pos, minFuelCost) = positionCosts.minBy(_._2)
  System.err.println(f"optimal position: $pos")
  println(minFuelCost)
}

def bad_argv1: Unit =
  System.err.println(santa_says)
  System.exit(0)
val santa_says = "Santa says: \"there\'s nothing here, you may want to try on channels 1 or 2.\""

// dynamic programming
object part1 {
  def apply(crabPositions: List[Int]): List[(Int, Int)] = {
    val crabPositionsSorted = crabPositions.sorted
    val leftmostPos = crabPositionsSorted(0)
    val nRightInitial = crabPositionsSorted.length
    val costRightInitial = part1.totalCost(crabPositionsSorted, leftmostPos - 1)
    return positionCosts(
      0, 0, leftmostPos, nRightInitial, costRightInitial, crabPositionsSorted
    )
  }

  def totalCost(crabPs: List[Int], alignPos: Int): Int =
    crabPs.map(_ - alignPos).map(_.abs).sum

  /** pre: nLeft & costLeft keep count of crabPs & cost on the left of pos
    *      nRight & costRight idem
    *      crabPs is sorted asc, only contains positions to the right of pos
    * ret: a List[(pos, cost)] from min crabPs to max crabPs
    */
  def positionCosts(
    nLeft: Int, costLeft: Int, pos: Int, nRight: Int, costRight: Int, crabPs: List[Int]
  ): List[(Int, Int)] = {
    if (crabPs.isEmpty) List.empty
    else {
      // cost + 1 for every crab on the left
      val costLeftNow = costLeft + nLeft
      // cost - 1 for every crab on the right
      val costRightNow = costRight - nRight
      val costNow = costLeftNow + costRightNow

      val nSamePosCrabs = crabPs.takeWhile(_ == pos).length
      val crabPsNext = crabPs.drop(nSamePosCrabs)
      val nLeftNext = nLeft + nSamePosCrabs
      val nRightNext = nRight - nSamePosCrabs

      (pos, costNow) :: positionCosts(
        nLeftNext, costLeftNow, pos + 1, nRightNext, costRightNow, crabPsNext
      )
    }
  }
}

// brute force
object part2 {
  def apply(crabPositions: List[Int]): List[(Int, Int)] = {
    val minCrabPos = crabPositions.min
    val maxCrabPos = crabPositions.max
    val candidatePs = (minCrabPos to maxCrabPos).toList
    candidatePs.zip(candidatePs.map(totalCost(crabPositions, _)))
  }

  def totalCost(crabsPs: List[Int], alignPos: Int) =
    crabsPs.map(cost(_, alignPos)).sum

  def cost(crabPos: Int, alignPos: Int): Int = {
    val dist = (alignPos - crabPos).abs
    dist * (dist + 1) / 2
  }
}
