package aoc

import scala.io.Source

object Day09 extends App{
  def partOne() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-09.txt").getLines().toList.mkString("")

    var leftIdx = 0
    var rightIdx = input.length

    val DEBUG = true
    var checksum = 0
    var spaceIdx = 0
    val output = new StringBuilder()
    case class Hanging(blockIdx: Int, remainingSize: Int)
    var hangingBlock = Hanging(0, 0)

    while(leftIdx < rightIdx){
      val isBlock = leftIdx % 2 == 0
      if(isBlock){
        val currSize = input(leftIdx).toString.toInt
        if(DEBUG){
          output.addAll((leftIdx / 2).toString.repeat(currSize))
        }
        spaceIdx += currSize
        leftIdx += 1
      }else{
        var freeSize = input(leftIdx).toString.toInt

        while(freeSize > 0 && leftIdx < rightIdx){
          if(hangingBlock.remainingSize > 0){
            if(DEBUG){
              output.addAll(hangingBlock.blockIdx.toString)
            }
            freeSize = freeSize - 1
            hangingBlock = hangingBlock.copy(remainingSize = hangingBlock.remainingSize - 1)
          }else{
            rightIdx = rightIdx - 1
            hangingBlock = Hanging(rightIdx / 2 , input(rightIdx).toString.toInt)
          }
        }
        leftIdx += 1
      }
    }

    val res = output.toString()
    val r = (0 until res.length).map(idx => idx * res(idx).toInt).sum

    print(res, r)
  }

  def partTwo() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-09.txt").getLines().map(_.split(" +").map(_.toInt))

  }

  partOne()
  partTwo()
}
