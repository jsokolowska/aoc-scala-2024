package aoc

import scala.io.Source

object Day07 extends App{
  def partOne() = {
    def input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-07.txt").getLines()
      .map(line => ("""\d+""".r findAllIn line).map(BigInt(_)).toList)
    val firstList = input.filter(
      lst => {
        val res = canBeProducedBF(lst.head, lst.tail)
        if(! res) println(s"Failing line: $lst")
        res
      }
    ).map(_.head).sum
    println(s"Result is: $firstList")

  }

    // todo figure out what we missing
  def canBeProduced(goal: BigInt, value: BigInt, remainingNumbers: List[BigInt]): Boolean = {
    if(value == goal){
      remainingNumbers.isEmpty
    }else if(remainingNumbers.isEmpty){
      false
    }else if(value > goal){
      false
    }else{
     val next = remainingNumbers.head
     canBeProduced(goal, value + next, remainingNumbers.tail)  || canBeProduced(goal, value * next, remainingNumbers.tail)
    }
  }

  def canBeProducedBF(goal: BigInt, numbers: List[BigInt]): Boolean = {
    val possibilities =gen(numbers.head, numbers.tail)

    possibilities.contains(goal)
  }

  def gen(value: BigInt, numbers: List[BigInt]): List[BigInt] = {
    val next = numbers.head
    if(numbers.size == 1){
      List(value + next, value * next)
    }else{
      gen(next * value, numbers.tail) ::: gen(next + value, numbers.tail)
    }
  }


  def partTwo() = {
    def input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-07.txt").getLines()
      .map(line => ("""\d+""".r findAllIn line).map(BigInt(_)).toList)
    val firstList = input.filter(
      lst => {
        val res = canBeProducedBF2(lst.head, lst.tail)
        if(! res) println(s"Failing line: $lst")
        res
      }
    ).map(_.head).sum
    println(s"Result is: $firstList")
  }


  def canBeProducedBF2(goal: BigInt, numbers: List[BigInt]): Boolean = {
    val possibilities =gen2(numbers.head, numbers.tail)

    possibilities.contains(goal)
  }

  def gen2(value: BigInt, numbers: List[BigInt]): List[BigInt] = {
    val next = numbers.head
    if(numbers.size == 1){
      List(value + next, value * next, BigInt(value.toString + next.toString))
    }else{
      gen2(next * value, numbers.tail) ::: gen2(next + value, numbers.tail) ::: gen2(BigInt(value.toString + next.toString), numbers.tail)
    }
  }

  //partOne()
  partTwo()
}
