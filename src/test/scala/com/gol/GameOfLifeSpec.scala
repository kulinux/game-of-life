package com.gol

import org.scalatest.{FlatSpec, Matchers}

class GameOfLifeSpec extends FlatSpec with Matchers {

  "A Grid" should "be built from arrays of arrays" in {

    Grid.Dim = 2

    val init = Seq(
      Seq(1, 1, 0),
      Seq(0, 0, 0),
      Seq(0, 0, 0)
    )

    var grid = Grid(init)

    grid.dumpInConsole()

    assert( grid.cell(0, 0).isDefined )
    assert( grid.cell(0, 0).get.content == Alive )
    assert( grid.cell(0, 1).get.content == Alive )
    assert( grid.cell(0, 2).get.content == Dead )
    assert( grid.cell(1, 0).get.content == Dead )

    assert( grid.cell(0, 0).get.n.size == 3 )
    assert( grid.cell(0, 1).get.n.size == 5 )
    assert( grid.cell(0, 2).get.n.size == 3 )
  }

  "A Grid" should "be build from next" in {

    Grid.Dim = 2

    val init = Seq(
      Seq(1, 1, 1),
      Seq(1, 1, 0),
      Seq(0, 0, 0)
    )

    var grid = Grid(init)
    grid.next.dumpInConsole()
  }

  "A Grid" should "support 20" in {

    Grid.Dim = 19

    val r = new scala.util.Random

    val init = Seq.fill(Grid.Dim + 1, Grid.Dim + 1){ r.nextInt(2)}

    var grid = Grid(init)
    1 to 100 foreach( x => {
      grid = Grid(init)
      grid.dumpInConsole()
    })

  }

}
