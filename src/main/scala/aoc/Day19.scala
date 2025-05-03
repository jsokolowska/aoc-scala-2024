package aoc

package aoc

import scala.io.Source

object Day19 extends App {
  def partOne() = {
    val input =  Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-19.txt").getLines().toList
    val (t, p) = input.span(!_.isBlank)

    val towels = t.flatMap(_.split(",").map(_.trim))
    val patterns = p.filter(!_.isBlank)

    var cache = Map.empty[String, Long]

    def canBeMade(pattern: String): Long = {
      if(cache.contains(pattern)){
        return cache(pattern)
      }
      val matches = towels.filter(towel => pattern.startsWith(towel))
      var result = 0l
      matches.foreach{
        m => {
          val newPattern = pattern.drop(m.length)
          if(newPattern.isEmpty){
            result = result + 1l
          }else{
            result += canBeMade(newPattern)
          }

        }
      }

        cache = cache + (pattern -> result)

      result
    }
    val results = patterns.zipWithIndex.map(pat => {
      println(s"${pat._2} Evaluating....${pat._1}")
        pat._1
    }) .map(canBeMade)
    println("Part one: " + results.sum)
  }




  partOne()

}

