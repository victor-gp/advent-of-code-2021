// https://adventofcode.com/2021/day/7

import scala.io.Source

@main def day07(): Unit = {
  val source = Source.fromFile("../../input/day07-in.txt")
  val input = source.mkString
  source.close

  val crabPositions = input.trim.split(",").map(_.toInt)
  val crabPositionsSorted = crabPositions.sorted.toList
  val leftmostPos = crabPositionsSorted(0)
  val nRightInitial = crabPositionsSorted.length
  val costRightInitial = totalCost(crabPositionsSorted, leftmostPos - 1)
  val positionFuelCosts = positionCosts(
    crabPositionsSorted, 0, 0, leftmostPos, nRightInitial, costRightInitial
  )
  val (pos, minFuelCost) = positionFuelCosts.minBy(_._2)
  System.err.println(f"optimal position: $pos")
  println(minFuelCost)
}

def totalCost(crabPs: List[Int], alignPos: Int): Int =
  crabPs.map(_ - alignPos).map(_.abs).sum

/** pre: crabPs is sorted asc
  *      nLeft & costLeft keep count of crabPs & cost on the left of pos
  *      nRight & costRight idem
  * ret: a List[(pos, cost)] from min crabPs to max crabPs
  */
def positionCosts(
  crabPs: List[Int], nLeft: Int, costLeft: Int, pos: Int, nRight: Int, costRight: Int
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
      crabPsNext, nLeftNext, costLeftNow, pos + 1, nRightNext, costRightNow
    )
  }
}
