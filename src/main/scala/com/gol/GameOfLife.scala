package com.gol


trait CellContent
case object Dead extends CellContent
case object Alive extends CellContent

object CellContent {
 def apply(x: Int)  = x match {
   case 0 => Dead
   case _ => Alive
 }
}

case class Cell(content: CellContent, val n: Seq[CellContent]) {

  def numberOfAlive(n: Seq[CellContent]) = n.count( _ == Alive )


  def next =  content match  {
    case Dead if(numberOfAlive(n)) == 3 => Cell(Alive, n)
    case Alive if( List(2, 3) contains numberOfAlive(n) ) => Cell(Alive, n)
    case _ => Cell(Dead, n)
  }

  override def toString = content.toString + " " + numberOfAlive(n)
}



class Grid(cells: Seq[Cell]) {

  def next = {
    new Grid( cells.map( _.next ) )
  }

  def cell(i: Int, j: Int) : Option[Cell] =
    if( Grid.inRange(i, j) ) Some(cells(i * Grid.Dim + j))
    else None

  def dumpInConsole() {
    cells.zipWithIndex.foreach {
      c => {
        if(c._2 % ( Grid.Dim + 1)  == 0 ) println
        if(c._1.content == Alive) print(" A ")
        if(c._1.content == Dead) print(" D ")
      }
    }
  }

}

object Grid {
  var Dim : Int = 0;

  private def inRange(i: Int, j: Int) =
    i >= 0 && i <= Dim && j >= 0 && j <= Dim

  def elem(init: scala.collection.mutable.Map[Tuple2[Int, Int], CellContent], i: Int, j: Int) : Option[CellContent] = {
    if( inRange(i, j) ) Some(init(i, j))
    else None
  }


  def apply(init: Seq[Seq[Int]]) : Grid = {
    val mapOfGridContent = scala.collection.mutable.Map[Tuple2[Int, Int], CellContent]()
    val mapOfGridNeigh = scala.collection.mutable.Map[Tuple2[Int, Int], Seq[CellContent]]()

    for(i <- 0 to Dim) for(j <- 0 to Dim) {
      mapOfGridContent((i, j)) = CellContent(init(i)(j))
    }

    for(i <- 0 to Dim) for(j <- 0 to Dim) {
     val neig = Seq(
       Grid.elem(mapOfGridContent, i - 1, j),
       Grid.elem(mapOfGridContent, i, j - 1 ),
       Grid.elem(mapOfGridContent, i + 1, j),
       Grid.elem(mapOfGridContent, i, j + 1 ),
       Grid.elem(mapOfGridContent, i - 1, j - 1),
       Grid.elem(mapOfGridContent, i - 1, j + 1),
       Grid.elem(mapOfGridContent, i + 1, j - 1),
       Grid.elem(mapOfGridContent, i + 1, j + 1),
     )

     val neigFilter : Seq[CellContent] = neig.filter(_.isDefined).map(_.get)
     mapOfGridNeigh((i, j)) = neigFilter
    }

    var mapOfGridSort = Seq[Cell]()
    for(i <- 0 to Dim) for(j <- 0 to Dim) {
     mapOfGridSort = mapOfGridSort :+ Cell(mapOfGridContent(i, j), mapOfGridNeigh(i, j))
    }
    new Grid(mapOfGridSort)

  }
}






