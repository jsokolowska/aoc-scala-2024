package aoc

import scala.collection.mutable
import scala.io.Source
import scala.sys.exit

object Day18 extends App {
  val gridMax = 70

  def partOne() = {
    val maxConsidered = 1024
    val input =  Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-18.txt").getLines()
      .map(
        line =>  {
          val splt = line.split(",")
          splt.head.toInt -> splt(1).toInt
    }).toList
    val startingPos = Point(0,0)
    var grid = new Array[Array[Char]](gridMax+ 1)
    grid = grid.map(_ => new Array[Char](gridMax + 1).map(_ => '.'))


    def fall(pos: (Int, Int), elem: Char = '#'): Unit = {
      grid(pos._2).update(pos._1, elem)
    }

    input.zipWithIndex.foreach(
      pair =>  if(pair._2 < maxConsidered) fall(pair._1)
    )
    case class Node(parent: Option[Node], point: Point, dist: Int, heuristic: Int){
      override def toString: String = s"$point: $f"
      def f: Int = dist + heuristic
    }
    def generateSuccessors(point: Point): List[Point] = {
      List(qualify(point.x, point.y - 1, grid), qualify(point.x, point.y + 1, grid),
        qualify(point.x + 1, point.y, grid), qualify(point.x - 1, point.y, grid)).flatten
    }

    def qualify(x: Int, y: Int, grid: Array[Array[Char]]): Option[Point] = {
      if (x >= 0 && x <= gridMax && y>= 0 && y<= gridMax && grid(y)(x) == '.'){
        Some(Point(x, y))
      }else{
        None
      }
    }
    def astar(): Node = {
      val endPos = Point(gridMax, gridMax)
      val openSet = mutable.Set(Node(None, startingPos, 0, startingPos.dist(endPos)))
      val closedSet = mutable.Set.empty[Node]

      var dest = Option.empty[Node]


      while (openSet.nonEmpty){
        val closestNode = openSet.minBy(pair => pair.f)
        //println(s"Node: $closestNode, to consider still: ${openSet.size}, already considered: ${closedSet.size}")
        openSet.remove(closestNode)
        if(openSet.size> gridMax * gridMax){
          println("AAA")
          exit(12)
        }
        val succesors = generateSuccessors(closestNode.point)
        for (consideredNode <- succesors) {
          if (consideredNode == endPos){

            //path found!
            dest = Some(Node(Some(closestNode), consideredNode, dist = closestNode.dist + 1, 0))
            return dest.get
          }else {
            val dist = closestNode.dist + 1
            val h = endPos.dist(consideredNode)
            val f = h + dist
            if (!openSet.exists(visitedNode => visitedNode.point == consideredNode && visitedNode.f <= f)
             &&  !closedSet.exists(pair => pair.point == consideredNode && pair.f <= f)
            ){
              openSet.add(Node(Some(closestNode), consideredNode, dist, h))
            }
          }
        }
        closedSet.add(closestNode)
      }
      null
    }

    println("With fallen snow")
    printArr(grid)

    val res = astar()

    var path = List.empty[Point]
    var count = 0
    var cNode = res
    while(cNode.parent.isDefined){
      count += 1
      path = path.appended(cNode.point)
      cNode = cNode.parent.get
    }
    // add path
    path = path.appended(startingPos)
    path.foreach{
      point => fall((point.x, point.y), 'O')
    }
    println("")
    println("With path")
    printArr(grid)

    println("Part one: " , count)
  }


  def partTwo() = {
    val maxConsidered = 1024
    val input =  Source.fromFile("C:\\Users\\Asia\\IdeaProjects\\aoc24\\src\\resources\\day-18.txt").getLines()
      .map(
        line =>  {
          val splt = line.split(",")
          splt.head.toInt -> splt(1).toInt
        }).toList
    val startingPos = Point(0,0)
    var grid = new Array[Array[Char]](gridMax+ 1)
    grid = grid.map(_ => new Array[Char](gridMax + 1).map(_ => '.'))


    def fall(pos: (Int, Int), elem: Char = '#'): Unit = {
      grid(pos._2).update(pos._1, elem)
    }

    input.zipWithIndex.foreach(
      pair =>  if(pair._2 < maxConsidered) fall(pair._1)
    )
    case class Node(parent: Option[Node], point: Point, dist: Int, heuristic: Int){
      override def toString: String = s"$point: $f"
      def f: Int = dist + heuristic
    }
    def generateSuccessors(point: Point): List[Point] = {
      List(qualify(point.x, point.y - 1), qualify(point.x, point.y + 1), qualify(point.x + 1, point.y), qualify(point.x - 1, point.y)).flatten
    }

    def qualify(x: Int, y: Int): Option[Point] = {
      if (x >= 0 && x <= gridMax && y>= 0 && y<= gridMax && grid(y)(x) == '.'){
        Some(Point(x, y))
      }else{
        None
      }
    }
    def astar(): Node = {
      val endPos = Point(gridMax, gridMax)
      val openSet = mutable.Set(Node(None, startingPos, 0, startingPos.dist(endPos)))
      val closedSet = mutable.Set.empty[Node]

      var dest = Option.empty[Node]


      while (openSet.nonEmpty){
        val closestNode = openSet.minBy(pair => pair.f)
        //println(s"Node: $closestNode, to consider still: ${openSet.size}, already considered: ${closedSet.size}")
        openSet.remove(closestNode)
        if(openSet.size> gridMax * gridMax){
          println("AAA")
          exit(12)
        }
        val succesors = generateSuccessors(closestNode.point)
        for (consideredNode <- succesors) {
          if (consideredNode == endPos){

            //path found!
            dest = Some(Node(Some(closestNode), consideredNode, dist = closestNode.dist + 1, 0))
            return dest.get
          }else {
            val dist = closestNode.dist + 1
            val h = endPos.dist(consideredNode)
            val f = h + dist
            if (!openSet.exists(visitedNode => visitedNode.point == consideredNode && visitedNode.f <= f)
              &&  !closedSet.exists(pair => pair.point == consideredNode && pair.f <= f)
            ){
              openSet.add(Node(Some(closestNode), consideredNode, dist, h))
            }
          }
        }
        closedSet.add(closestNode)
      }
      null
    }

    println("With fallen snow")
    printArr(grid)

    val res = astar()

    var path = List.empty[Point]
    var count = 0
    var cNode = res
    while(cNode.parent.isDefined){
      count += 1
      path = path.appended(cNode.point)
      cNode = cNode.parent.get
    }
    // add path
    path = path.appended(startingPos)
    path.foreach{
      point => fall((point.x, point.y), 'O')
    }
    println("")
    println("With path")
    printArr(grid)

    println("Part one: " , count)
  }


  def printArr(arr: Array[Array[Char]]) = {
    for (line <- arr) {
      println(line.mkString(""))

    }
  }



  partOne()

}

