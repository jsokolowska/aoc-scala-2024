package aoc

import scala.io.Source
import scala.math.abs

object Day02 extends App {
  def partOne() = {

    val result = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-02.txt").getLines()
      .map(_.split(" +").map(_.toInt))
      .filter(isSafe(_))

    println(s"Result part two: ${result.size}")
  }

  def isSafe(levels: Seq[Int]): Boolean = (allIncreasing(levels) || allDecreasing(levels)) && innerDelta(levels)

  def allIncreasing(levels: Seq[Int]): Boolean = {
    levels.sorted == levels
  }

  def allDecreasing(levels: Seq[Int]): Boolean = {
    levels.sorted.reverse == levels
  }

  def innerDelta(levels: Seq[Int], min: Int = 1, max: Int = 3) = {
    levels.foldLeft((true, levels.head - 1))(
      (soFar, current) => {
        val (previousSafe, last) = soFar
        val delta = abs(current - last)
        val currentSafe = min <= delta && delta <= max
        (previousSafe && currentSafe, current)
      }
    )._1
  }

  def partTwo() = {
    val result =  Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-02.txt").getLines().map(_.split(" +").map(_.toInt).toList)
      .filter(generateRemoved(_).exists(isSafe(_)))
    println(s"Result part two: ${result.size}")
  }

  def generateRemoved(levels: List[Int]) : List[List[Int]] = {
    levels.indices.map(i => levels.take(i) ++ levels.drop(i + 1)).toList
  }

  partOne()
  partTwo()
}
