package aoc

import scala.io.Source

object Day11 extends App{
  def partOne() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-11.txt").getLines().flatMap(_.split("\\s").map(_.toInt).map(sth => BigInt(sth))).toList

    val blink = 75
    var stones = input
    (1 to blink).foreach (
      idx => {
        stones = stones.flatMap(change)
        println(s"[$idx]")
      }
    )


    print(stones.length)
  }

  def partTwo() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-11.txt").getLines().map(_.split(" +").map(_.toInt))

  }

  def change(stone: BigInt): List[BigInt] = {
    val sLen = stone.toString.length
    if(stone == 0){
      List(1)
    }else if(sLen % 2 == 0){
      val left = stone.toString.substring(0, sLen/2)
      val right = stone.toString.substring(sLen/2)

      List(BigInt(left), BigInt(right))
    }else{
      List(stone * 2024)
    }
  }

  partOne()
  partTwo()
}
