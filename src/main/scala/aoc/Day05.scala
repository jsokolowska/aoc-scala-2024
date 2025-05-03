package aoc

import scala.io.Source

object Day05 extends App {
  def partOne() = {
    val (rules, updates) = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-05.txt").getLines().span(!_.isBlank)
    val ruleList = rules.toList

    val result = updates.filter(!_.isBlank).map(_.split(",").map(_.toInt)).filter(update => {
      !ruleList.flatMap(rule => fulfillsRule(rule, update)).exists(!_)
    }).map(lst => {
     // println(lst.mkString(", "))
      val idx = (lst.length - 1) / 2
      lst(idx)
    }).sum

    println("Part one: " + result)
  }

  def fulfillsRule(rule: String, update: Array[Int]): Option[Boolean] = {
    rule.split("\\|").map(_.toInt).toList match {
      case left :: right :: Nil =>
        val lIdx = update.indexOf(left)
        val rIdx = update.indexOf(right)
        if (lIdx < 0 || rIdx < 0) {
          Some(true)
        } else {
          if(lIdx >= rIdx){
            //println(s"Update ${update.mkString(", ")} violates rule $rule")
          }
          Some(lIdx < rIdx)
        }
      case _ => None
    }
  }

  def partTwo() = {
    val (rules, updates) = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-05.txt").getLines().span(!_.isBlank)
    val ruleList = rules.toList

    val incorrectUpdates = updates.filter(!_.isBlank).map(_.split(",").map(_.toInt))
      .filter(update => {
        ruleList.flatMap(rule => fulfillsRule(rule, update)).exists(!_)
      }).map(update => {
      (update, ruleList.filter(rule => isAffectedByRule(rule, update)))
    }).filter(_._2.nonEmpty)
      .map(pair => reorder(pair._2, pair._1)).map(
        lst =>
          {
            //println("Reordered update ", lst.mkString(","))
            val idx = (lst.length - 1) / 2
            lst(idx)
          }
      ).sum

    println("Part two: ", incorrectUpdates )
  }

  def isAffectedByRule(rule: String, update: Array[Int]): Boolean = {
    rule.split("\\|").map(_.toInt).toList match {
      case left :: right :: Nil =>
        val lIdx = update.indexOf(left)
        val rIdx = update.indexOf(right)
        !(lIdx < 0 || rIdx < 0)
      case _ => false
    }
  }

  def reorder(rules: Seq[String], update: Seq[Int]): List[Int] = {
    val parsedRules = rules.map(_.split("\\|").map(_.toInt).toList match {
      case left :: right :: Nil => (left, right)
    }).toList
    findNextRec(parsedRules, update.toList)
  }

  def findNextRec(rules: List[(Int, Int)], remaining: List[Int]): List[Int] = {
    if(remaining.isEmpty){
      return remaining
    }
    val numbersJustOnLeftSide = remaining.filter(num => !rules.exists(rule => rule._2 == num))
    val remainingRules = rules.filter(rule => !numbersJustOnLeftSide.contains(rule._1))
    val remainingNumbers = remaining.filter(!numbersJustOnLeftSide.contains(_))
    numbersJustOnLeftSide ::: findNextRec(remainingRules, remainingNumbers)
  }

  //partOne()
  partTwo()
}
