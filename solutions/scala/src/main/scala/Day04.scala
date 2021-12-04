import scala.io.Source
import scala.collection.immutable.HashMap
import scala.collection.mutable.ArraySeq


@main def day04(part: Int): Unit =
  val bufferedSource = Source.fromFile("../../input/day04-in.txt")
  val input = bufferedSource.mkString
  bufferedSource.close

  val blocks = input.split("\n\n")
  val numbers = blocks.head.split(',').toList.map(_.toInt)
  val boards = blocks.toList.tail.map(BingoBoard.fromString(_))
  part1(numbers, boards)


def part1(numbers: List[Int], boards: List[BingoBoard]): Unit =
  val (winningBoard, bingoNum) = play(boards, numbers)
  println(winningBoard.score(bingoNum))

def play(boards: List[BingoBoard], numbers: List[Int]): (BingoBoard, Int) =
  val nextNum = numbers.head
  val containing = boards.filter(_.contains(nextNum))
  containing.foreach(_.markNumber(nextNum))

  val winnerOpt = containing.find(_.hasWonAfter(nextNum))
  winnerOpt match
    case Some(winner) => (winner, nextNum)
    case None => play(boards, numbers.tail)


type Pos2D = (Int, Int)
type Grid[T] = ArraySeq[ArraySeq[T]]

class BingoBoard(val numToPos: HashMap[Int, Pos2D], var marked: Grid[Boolean]):
  import BingoBoard.*

  def contains(num: Int): Boolean = numToPos.contains(num)

  // pre: the board contains num
  // post: mutates marked
  def markNumber(num: Int): Unit =
    val (row, col) = numToPos(num)
    marked(row)(col) = true

  def hasWonAfter(num: Int): Boolean =
    val (row, col) = numToPos(num)
    isCompleteRow(row) || isCompleteColumn(col)

  def isCompleteRow(row: Int): Boolean = marked(row).forall(_ == true)

  def isCompleteColumn(col: Int): Boolean = false //todo

  def score(bingoNum: Int): Int =
    val unmarkedNums =
      for
        num <- numToPos.keys
        (row, col) = numToPos(num)
        if ! marked(row)(col)
      yield
        num
    unmarkedNums.sum * bingoNum

object BingoBoard:
  // boards are always 5x5
  val sideLength: Int = 5

  def fromString(grid: String): BingoBoard =
    val numToPos = HashMap.from(
      for
        (row, i) <- grid.linesIterator.zipWithIndex
        (numStr, j) <- row.trim.split("\\s+").zipWithIndex
      yield
        (numStr.toInt, (i, j))
    )
    val marked = ArraySeq.fill(sideLength, sideLength){ false }
    BingoBoard(numToPos, marked)
