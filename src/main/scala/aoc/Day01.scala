package aoc

import scala.io.Source
import scala.math.abs

object Day01 extends App {
  def partOne() = {
    def input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-01.txt").getLines().map(_.split(" +").map(_.toInt))
    val firstList = input.map(_.head).toList.sorted
    val secondList = input.map(_.last).toList.sorted
    val diffs = (firstList zip secondList).map(pair => abs(pair._1 - pair._2))
    println("Part one: " + diffs.sum)
  }

  def partTwo() = {
    def input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-01.txt").getLines().map(_.split(" +").map(_.toInt))
    val firstList = input.map(_.head).toList.sorted
    val occurenceCount = input.map(_.last).toList.groupBy(identity).view.mapValues(_.size)
    val diffs = firstList.map(elem => occurenceCount.getOrElse(elem, 0) * elem)
    println("Part two: " + diffs.sum)
  }

  partOne()
  partTwo()
}
