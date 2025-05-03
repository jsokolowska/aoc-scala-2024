package aoc

import scala.io.Source

object Day22 extends App {
  def partOne() = {
    val input =  Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-22.txt")
      .getLines().map(_.toInt).map(i => BigInt(i))

    val res = input.map{
      num => gen(num, 2000)
    }.sum

    println("Res Part one: " + res)
  }

  def gen(secretNumber: BigInt, times: Int) = {
    var res = secretNumber;
    (0 until times).foreach{
      idx => {
        res = nextSecret(res)
        //println(s"$idx - $res")
      }
    }
    res
  }

  def nextSecret(secretNumber: BigInt): BigInt = {
    val res = secretNumber  << 6
    val secretMixed = mixAndPrune(secretNumber, res)

    val div = secretMixed >> 5
    val secondMixedSecret = mixAndPrune(secretMixed, div)

    val last = secondMixedSecret << 11
    mixAndPrune(last, secondMixedSecret)

  }

  def mixAndPrune(input: BigInt, number: BigInt): BigInt = {
    val mixed = input ^ number

    val moduloFactor = 0x0FFFFFF
    mixed & moduloFactor
  }

  partOne()
}
