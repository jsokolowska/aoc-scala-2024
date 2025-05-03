package aoc

import scala.io.Source

object Day03 extends App{
  def partOne() = {
    val re = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)".r
    val result = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-03.txt").getLines().toList
      .map(line => {

        val matches = re.findAllIn(line)
        matches.matchData.map(_.subgroups.foldLeft(1)((mult, curr) => mult * curr.toInt)).sum
      }).sum

    println(s"Result part one: ${result}")
  }

  def partTwo() = {
    val re = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)|do\\(\\)|don't\\(\\)".r
    var enabled = true
    val result = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-03.txt").getLines().toList
      .map(line => {

        val matches = re.findAllIn(line)
        matches.matchData.map(aMatch => {
          if(aMatch.source.subSequence(aMatch.start, aMatch.end) == "do()"){
            enabled = true
            0
          }else if(aMatch.source.subSequence(aMatch.start, aMatch.end) == "don't()"){
            enabled = false
            0
          }else if(enabled){
            aMatch.subgroups.foldLeft(1)((mult, curr) => mult * curr.toInt)
          }else{
            0
          }
        }).sum
      }).sum

    println(s"Result part one: ${result}")
  }



  partOne()
  partTwo()
}
