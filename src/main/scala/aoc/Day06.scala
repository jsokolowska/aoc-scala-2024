package aoc

import scala.io.Source
import scala.util.Using

object Day06 extends App {
  def partOne = {
    val lines = Map2D.apply(
      "C:\\Users\\Asia\\IdeaProjects\\aoc-scala-2024\\src\\resources\\day-06.txt"
    )
   
    val (initialGuardPos, initialDir) = lines.findFirst('^').map((_, Up))
      .orElse(lines.findFirst('>').map((_, Right)))
      .orElse(lines.findFirst('<').map((_, Left)))
      .orElse(lines.findFirst('>').map((_, Down)))   .get

    var visited = List.empty[(Int, Int)]

    var currentGuardPos = (initialGuardPos.posX, initialGuardPos.posY)
    //println(s"Initial: $currentGuardPos")
    var currentDirection: Direction = initialDir

    var isOnMap = true
    while(isOnMap) {

      def nextIdx(): (Int, Int) =
        currentDirection match {
          case Up => (currentGuardPos._1 - 1, currentGuardPos._2)
          case Right => (currentGuardPos._1, currentGuardPos._2 + 1)
          case Down => (currentGuardPos._1 + 1, currentGuardPos._2)
          case Left => (currentGuardPos._1, currentGuardPos._2 - 1)
        }

      visited = visited.appended(currentGuardPos)
      val (nextX, nextY) = nextIdx()
      if (lines.inBounds(nextX, nextY)) {
        val nextValue = lines(nextX, nextY)
        if (nextValue != '#') {
          currentGuardPos = (nextX, nextY)
        } else {
          currentDirection = currentDirection.turnRight
          //println(s"Obstacle - turning $currentDirection")
        }
      } else {
        //println(s"No longer in bounds: $nextX, $nextY")
        isOnMap = false
      }
    }
    //println(currentGuardPos)
    println(s"Visited distinct positions: ${visited.toSet.size}")
  }

  partOne
}

sealed trait Direction {
  def turnRight: Direction
}
case object Right extends Direction {
  override def turnRight: Direction = Down
}
case object Left extends Direction {
  override def turnRight: Direction = Up
}
case object Up extends Direction {
  override def turnRight: Direction = Right
}

case object Down extends Direction {
  override def turnRight: Direction = Left
}
class Map2D[T](private val field: Array[Array[T]]) {
  case class Field[X](elem: X, posX: Int, posY: Int)

  def findFirst(targetElem: T): Option[Field[T]] = {
    field.zipWithIndex.flatMap { case (e, xIdx) =>
      e.zipWithIndex
        .find { case (elem, _) => elem == targetElem }
        .map { case (elem, yIdx) => Field(elem, xIdx, yIdx) }
    }.headOption
  }

  def move(from: (Int, Int), to: (Int, Int), defaultValue: T): Unit = {
    val original = field(from._1)(from._2)
    this.field(from._1).update(from._2, defaultValue)
    this.field(to._1).update(to._2, original)
  }

  def inBounds(x: Int, y: Int): Boolean =
    0 <= x && x < field.length && 0 <= y && y < field(0).length

  def apply(x: Int, y: Int): T = field(x)(y)
}

object Map2D {
  def apply(from: String): Map2D[Char] = {
    val lines = Using(Source.fromFile(from)) { source =>
      source.getLines().toArray
    }.getOrElse(throw new IllegalStateException("Could not find file"))
    val initialMap = lines.map(line => line.toCharArray)
    new Map2D(initialMap)
  }
}
