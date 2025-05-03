package aoc


case class Point(x: Int, y: Int){
  def dist(p: Point): Int = Math.abs(this.x - p.x) + Math.abs(this.y - p.y)
  def successors2D: List[Point] = List(this.copy(y = y +1), this.copy(y = y-1), this.copy(x = x+1), this.copy(x = x-1))
}

object Util {

}
