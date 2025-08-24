package aoc

import scala.io.Source

object Day12 extends App{
  case class Point(x: Int, y:Int){
    def neighbours(xBound: Int, yBound: Int): Set[Point] =
      neighbours()
        .filterNot(p => p.x >= xBound | p.y >= yBound)
        .filterNot(p => p.x <0 | p.y < 0)

    def neighbours(): Set[Point] =
      Set(Point(x, y - 1), Point(x, y + 1), Point(x - 1, y), Point(x + 1, y))

    override def toString: String = s"($x,$y)"
  }
  case class Map2D(grid: List[List[Char]]){
    def xBound: Int = grid.length
    def yBound: Int = grid.head.length
    def apply(p: Point): Char = grid(p.x)(p.y)
    def unclaimedRegion(regions: Set[Region]): Option[Region] = {
      0.until(xBound).map(
        x => 0.until(yBound).map(
          y => if(!regions.exists(r => r.has(Point(x, y)))){
            return Some(Region(grid(x)(y), Set(Point(x,y))))
          }
        )
      )
      None
    }
  }
  case class Region(plant: Char, points: Set[Point]){
    private var canBeExtended = true
    def extend(m: Map2D): Region = {
      val extensionPoints = points.flatMap(_.neighbours(m.xBound, m.yBound)).diff(points)
      val matchedPoints = extensionPoints.filter(p => m(p) == plant)
      if (matchedPoints.isEmpty) {
        canBeExtended = false
        return this
      }
      Region(plant, points ++ matchedPoints)
    }

    def canExtend(): Boolean = canBeExtended

    def has(p: Point): Boolean = points.contains(p)

    override def toString: String = {
      s"$plant: [${points.mkString(", ")}]"
    }

    def area = points.size

    def perimeter = points.toSeq.map(_.neighbours().diff(points).size).sum
  }


  //todo this is brute force and not too efficient
  // result: 1344578
  // alternate idea - create horizontal slices, try connecting slices
  def partOne() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc-scala-2024\\src\\resources\\day-12.txt").getLines()
      .map(_.toCharArray.filterNot(_.isWhitespace).toList).toList
//    val input = """RRRRIICCFF
//                  |RRRRIICCCF
//                  |VVRRRCCFFF
//                  |VVRCCCJFFF
//                  |VVVVCJJCFE
//                  |VVIVCCJJEE
//                  |VVIIICJJEE
//                  |MIIIIIJJEE
//                  |MIIISIJEEE
//                  |MMMISSJEEE""".stripMargin.split("\n")
//             .map(_.toCharArray.filterNot(_.isWhitespace).toList).toList
    val map = Map2D(input)
    //initial
    val p = map(Point(0, 0))
    var region: Option[Region] = Some(Region(p, Set(Point(0, 0))))
    var regions = Set.empty[Region]

    while(region.isDefined){
      //println(s"Processing region... ${region.get}")
      region match {
        case Some(r) => if(r.canExtend()){
          region = Some(r.extend(map))
          //println(s"Extended region... ${r}")
        }else{
          regions = regions + r
          region = map.unclaimedRegion(regions)
        }
      }
    }

    val cost = regions.toSeq.map(r => r.area * r.perimeter).sum
    //regions.toSeq.sortBy(_.plant).foreach(r => println(r + s"\n\tArea: ${r.area}, perimeter: ${r.perimeter}"))
    println(s"Total cost: $cost")
  }

  def partTwo() = {
    val input = Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-12.txt").getLines().map(_.split(" +").map(_.toInt))

  }



  partOne()
  //partTwo()
}