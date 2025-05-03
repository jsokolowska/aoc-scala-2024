package aoc

import scala.io.Source

object Day04 extends App{
  var allCoords: List[(Int, Int)] = List()
  var xFound: List[(Int, Int)] = List()
  def partOne() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-04.txt").getLines().map(_.toCharArray).toArray

    val xSize = input.head.length
    val ySize = input.length
    val result = (0 until(xSize)).map(
      xPos => {
        (0 until ySize).map(
          yPos =>
            {
              val curr = input(yPos)(xPos)
              if(curr == 'X') {
                xFound = xFound.appended((yPos, xPos))
                endingStrings(xPos, yPos, input)
              }else{
                0
              }
            }
        ).sum
      }
    ).sum
    //printAll(allCoords, input)

    println(s"Result part one: ${result}")
    //printAll(xFound, input)
  }

  def draw(idxes: List[(Int, Int)], input: Array[Array[Char]]): Array[Array[Char]] = {
    input.zipWithIndex.map{
      case (line, idx2) =>
        line.zipWithIndex.map{
          case (char, secondIdx) =>
            if(idxes.contains(idx2, secondIdx)){
              char
            }else{
              '.'
            }
        }

    }.toArray
  }

  def printAll(idxes: List[(Int, Int)], input: Array[Array[Char]]): Unit = {
    val a = draw(idxes, input)
    a.foreach(line => println(line.mkString))
  }

  def endingStrings(xPos: Int, yPos: Int, input: Array[Array[Char]])  = {
    var result = 0
    if(xPos == 5){
      print('A')
    }
    if(checkPos((0 until 4).map(idx => (yPos, xPos+idx)), input)){
      result = result + 1
    }
    if(checkPos((0 until 4).map(idx => (yPos, xPos-idx)), input)){
      result = result + 1
    }
    if (checkPos((0 until 4).map(idx => (yPos + idx,xPos)), input)) {
      result = result + 1
    }
    if (checkPos((0 until 4).map(idx => (yPos - idx, xPos)), input)) {
      result = result + 1
    }
    if (checkPos((0 until 4).map(idx => (yPos - idx,xPos - idx)), input)) {
      result = result + 1
    }
    if (checkPos((0 until 4).map(idx => (yPos - idx,xPos + idx)), input)) {
      result = result + 1
    }
    if (checkPos((0 until 4).map(idx => (yPos + idx,xPos + idx)), input)) {
      result = result + 1
    }
    if (checkPos((0 until 4).map(idx => (yPos + idx,xPos - idx)), input)) {
      result = result + 1
    }
    result
  }

  def checkPos(pos: Seq[(Int, Int)], input: Array[Array[Char]], str: String = "XMAS"): Boolean = {
    val xSize = input.head.length
    val ySize = input.length
    if(!pos.exists(coord => coord._1 < 0 || coord._1 >= xSize || coord._2 < 0 || coord._2 >= ySize)){
      val cont = pos.map(idx => input(idx._1)(idx._2)).mkString
      val r = cont == str || cont == str.reverse
      if(r){
        allCoords = allCoords.appendedAll(pos)
        println(s"Coord: ${pos}")
      }
      r
    }else{
      false
    }
  }

  def partTwo() = {
    allCoords= List()
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-04.txt").getLines().map(_.toCharArray).toArray

    val xSize = input.head.length
    val ySize = input.length
    val result = (0 until(xSize)).map(
      xPos => {
        (0 until ySize).map(
          yPos =>
          {
            val curr = input(yPos)(xPos)
            if(curr == 'A') {
              xFound = xFound.appended((yPos, xPos))
              xMas(xPos, yPos, input)
            }else{
              false
            }
          }
        ).count(_ == true)
      }
    ).sum
    println(s"Result part two: ${result}")

    printAll(allCoords, input)
  }

  def xMas(xPos: Int, yPos: Int, input: Array[Array[Char]]): Boolean = {
    var result = 0
    if (yPos == 2) {
      print('A')
    }
    //    if(checkPos((0 until 4).map(idx => (yPos, xPos+idx)), input)){
    //      result = result + 1
    //    }
    //    if(checkPos((0 until 4).map(idx => (yPos, xPos-idx)), input)){
    //      result = result + 1
    //    }
    val positions = List((-1 to 1).map(idx => (yPos - idx, xPos + idx)), (-1 to 1).map(idx => (yPos + idx, xPos + idx)), (-1 to 1).map(idx => (yPos, xPos + idx)),
      (-1 to 1).map(idx => (yPos, xPos + idx))).map(_.toArray).toArray
    checkPos((-1 to 1).map(idx => (yPos - idx, xPos + idx)), input, "MAS") &&
      checkPos((-1 to 1).map(idx => (yPos + idx, xPos + idx)), input, "MAS")
  }
  partOne()
  partTwo()
}
