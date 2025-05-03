package aoc

import scala.io.Source



object Day20 extends App{
  def partOne() = {
    val input =  Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-20.txt").getLines().toList
    val raceGrid = input.map(_.toArray).toArray

    def getPath(startChar: Char = 'S', endChar: Char = 'E', freeChar: Char = '.') = {
      val startPos = findElem(startChar, raceGrid)
      val endPos = findElem(endChar, raceGrid)

      var path = List.empty[Point]
      var pos = startPos
      while(pos != endPos){
        val successors = pos.successors2D.filter(p => raceGrid(p.y)(p.x) == freeChar || raceGrid(p.y)(p.x) == endChar).filter(p => !path.contains(p))
        if(successors.size > 1){
          throw new IllegalStateException(s"More than one possible path from $pos")
        }
        if(successors.isEmpty){
          throw new IllegalStateException(s"No path at $pos")
        }
        path = path.appended(pos)
        pos = successors.head

      }
      path.appended(pos)
    }

    val path = getPath()

    println("Part one: " + path.length )
  }

  def findElem(ch: Char, grid: Array[Array[Char]]): Point = {
    grid.indices.foreach{
      idx => {
        val xIdx = grid(idx).zipWithIndex.filter(withIdx => withIdx._1 == ch).map(_._2)
        if(xIdx.nonEmpty){
          return Point(x = xIdx.head, idx)
        }

      }
    }
    throw new IllegalStateException(s"Couldnt find char $ch")
  }

  partOne()
}
