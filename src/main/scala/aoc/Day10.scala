package aoc

import scala.io.Source

object Day10 extends App{
  def partOne() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-10.txt").getLines().map(_.map(_.toString.toInt).toArray).toArray

    val startingPositions = input.indices.flatMap(
      idx => input(idx).indices.filter(xIdx => input(idx)(xIdx) == 0).map((idx, _))
    ).map(
      position => {
        val paths = generatePaths(input, position)
        paths.toSet.size
      }
    ).sum

    println(startingPositions)
  }



  def partTwo() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-10.txt").getLines().map(_.map(_.toString.toInt).toArray).toArray

    val startingPositions = input.indices.flatMap(
      idx => input(idx).indices.filter(xIdx => input(idx)(xIdx) == 0).map((idx, _))
    ).map(
      position => {
        val paths = generatePaths(input, position)
        paths.size
      }
    ).sum

    println(startingPositions)

  }

  def generatePaths(map: Array[Array[Int]], position: (Int, Int)): List[(Int, Int)]  = {
    def isLegal(position: (Int, Int)): Boolean = {
      !(position._1 < 0 || position._2 < 0 || position._1 >= map.length || position._2 >= map.head.length)
    }
    def generatePossibleMoves(position: (Int, Int)) = {
      val currVal = map(position._1)(position._2)
      List(
        (position._1, position._2 - 1),
        (position._1, position._2 + 1),
        (position._1 + 1, position._2),
        (position._1 - 1, position._2)
      ).filter(isLegal).filter(p => map(p._1)(p._2) == currVal + 1)
    }

    if(map(position._1)(position._2) == 9){
      List(position)
    }else{
      generatePossibleMoves(position).flatMap(pos => generatePaths(map, pos))
    }
  }


  partOne()
  partTwo()
}
