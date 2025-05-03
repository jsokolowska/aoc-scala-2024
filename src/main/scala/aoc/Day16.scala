package aoc

import scala.io.Source

object Day16 extends App {
  def partOne() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-16.txt").getLines().toList
    val (registers, program) = input.span(!_.isBlank)
    val regist = registers.map(
      registerRecord => {
        val parts = registerRecord.split(" ")
        (parts(1)(0), parts(2).toInt)
      }
    ).toMap

    var regA = regist('A')
    var regB = regist('B')
    var regC = regist('C')

    val prog = program(1).split(" ").last.split(",").filter(!_.isBlank).map(_.toInt)
    var output = List.empty[Int]

    def exec(opcode: Int, operand: Int, index: Int): Int = {
      var nextIdx = index + 2;
      val comboValue = operand match {
        case op if op <= 3 && op >= 0 => op
        case 4 => regA
        case 5 => regB
        case 6 => regC
      }

      opcode match {
        case 0 =>
          val res = regA / Math.pow(2, comboValue)
          regA = Math.floor(res).toInt
        case 1 =>
          regB = regB ^ operand
        case 2 =>
          regB = comboValue % 8
        case 3 =>
          if (regA != 0) {
            nextIdx = operand
          }
        case 4 =>
          regB = regB ^ regC
        case 5 =>
          output = output.appended(comboValue % 8)
        case 6 =>
          val res = regA / Math.pow(2, comboValue)
          regB = Math.floor(res).toInt
        case 7 =>
          val res = regA / Math.pow(2, comboValue)
          regC = Math.floor(res).toInt
      }
      nextIdx
    }

    var pointer = 0

    while (pointer < prog.length) {
      val opCde = prog(pointer)
      val opRnd = prog(pointer + 1)
      pointer = exec(opCde, opRnd, pointer)
    }

    println("Part one: " + output.mkString(","))
  }


  partOne()

}
