package aoc

import scala.io.Source

object Day08 extends App {
  case class Antenna(strength: Char)
  case class Point(x: Int, y: Int, structures: Option[Antenna] =None)

  def partOne() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-08.txt").getLines()

    val map = input.zipWithIndex.map {
      case  (line, lineIdx) => line.zipWithIndex.map{
        case ('.', idx) => Point(lineIdx, idx)
        case (strength, idx) => Point(lineIdx, idx, Some(Antenna(strength)))
      }.toArray
    }.toArray


    val maxX = map.length
    val maxY = map.head.length

    val antennaPoint = map.flatMap(_.filter{
      case Point(_, _, str) => str.nonEmpty
    })

    val res = antennaPoint.flatMap(
      p1 => {
        val antennaStr = p1.structures.map{
          case Antenna(a) => a
        }.head
        val matchingAnt = antennaPoint.filter(p2 => p2.x != p1.x && p2.y != p1.y).filter{
          ap => ap.structures.exists {
            case Antenna(a) => a == antennaStr
            case _ => false
          }
        }
        val ints = matchingAnt.flatMap( interfere(p1, _)).filter{
          case (x, y) => x>= 0 && x < maxX && y>= 0 && y < maxY
        }
        ints
      }
    ).toSet


    println(s"Size: ${res.size}")

  }

  def partTwo() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-08.txt").getLines()

    val map = input.zipWithIndex.map {
      case  (line, lineIdx) => line.zipWithIndex.map{
        case ('.', idx) => Point(lineIdx, idx)
        case (strength, idx) => Point(lineIdx, idx, Some(Antenna(strength)))
      }.toArray
    }.toArray


    val maxX = map.length
    val maxY = map.head.length

    val antennaPoint = map.flatMap(_.filter{
      case Point(_, _, str) => str.nonEmpty
    })

    val res = antennaPoint.flatMap(
      p1 => {
        val antennaStr = p1.structures.map{
          case Antenna(a) => a
        }.head
        val matchingAnt = antennaPoint.filter(p2 => p2.x != p1.x && p2.y != p1.y).filter{
          ap => ap.structures.exists {
            case Antenna(a) => a == antennaStr
            case _ => false
          }
        }
        val ints = matchingAnt.flatMap( interfere2(p1, _, (maxX, maxY)))
        ints
      }
    ).toList

    val ap = antennaPoint.map(a => (a.x, a.y)).toList

    val all = (res ::: ap).toSet


    println(s"Size: ${all.size}")

  }
  private def interfere(a: Point, b: Point): List[(Int, Int)]= {
    val diffX = a.x - b.x
    val diffY = a.y - b.y
    List( (a.x + diffX, a.y + diffY), (b.x - diffX, b.y - diffY)   )
  }

  private def interfere2(a: Point, b: Point, max: (Int, Int)): List[(Int, Int)]= {
    val diffX = a.x - b.x
    val diffY = a.y - b.y
    generate((a.x, a.y), (diffX, diffY), max) ::: generate((b.x, b.y), (-diffX, -diffY), max)
  }

  private def generate(a: (Int, Int), diff: (Int, Int), max: (Int, Int)): List[(Int, Int)] = {
    val newPoint = (a._1 + diff._1, a._2 + diff._2)
    if(newPoint._1 >= 0 && newPoint._2 >= 0 && newPoint._1 < max._1 && newPoint._2 < max._2){
      newPoint :: generate(newPoint, diff, max)
    }else{
      List.empty
    }
  }

  partOne()
  partTwo()
}
