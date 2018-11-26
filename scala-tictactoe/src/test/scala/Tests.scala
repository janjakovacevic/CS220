class Tests extends org.scalatest.FunSuite {

  import Solution._
  import hw.tictactoe._

  val game1 = new Game(O, 3, Map((0, 0) -> O, (0, 2) -> X, (1, 0) -> X,
                                (1, 1) -> X, (2, 0) -> O)) //not finished
  val game2 = new Game(O, 3, Map((0, 0) -> O, (0, 1) -> O, (0, 2) -> X,
                                (1, 1) -> X, (1, 2) -> X, (2, 0) -> X, 
                                (2, 2) -> O)) //X wins
  val game3 = new Game(O, 3, Map((0, 0) -> X, (0, 1) -> X, (0, 2) -> O,
                                (1, 0) -> O, (1, 1) -> X, (1, 2) -> X,
                                (2, 0) -> X, (2, 1) -> O, (2, 2) -> O)) //draw
  val game4 = new Game(X, 3, Map((1, 1) -> O, (1, 0) -> X, (0, 2) -> O,
                                (2, 0) -> X, (2, 2) -> O, (1, 2) -> X,
                                (0, 0) -> O)) //O wins
  val game5 = new Game(O, 3, Map((0, 1) -> O, (0, 2) -> X, (1, 1) -> X,
                                (2, 0) -> O, (2, 2) -> X)) //not finished, but X should win
  val game6 = new Game(X, 3, Map((0, 0) -> O, (0, 1) -> X, (1, 1) -> O, 
                                (2, 0) -> O, (2, 2) -> X)) //not finished, but O should win
  
  test("isFinished test1") {
    assert(game1.isFinished() == false)
    }

  test("minimax test1") {
    assert(minimax(game1) == None)
  }

  test("minimax test2") {
    assert(minimax(game2) == Some(X))
  }

  test("minimax test3") {
    assert(minimax(game3) == None)
  }

  test("minimax test4") {
    assert(minimax(game4) == Some(O))
    }

  test("minimax test5") {
    assert(minimax(game5) == Some(X))
    }

  test("minimax test6") {
    assert(minimax(game6) == Some(O))
    }

  val t1 = Map((0, 0) -> O, (0, 1) -> X, (0, 2) -> X, (1, 0) -> O, (1, 1) -> X, (1, 2) -> O, (2, 1) -> O, (2, 2) -> X)
  val t2 = Map((0, 0) -> O, (0, 1) -> X, (0, 2) -> X, (1, 0) -> O, (1, 1) -> X, (1, 2) -> O, (2, 0) -> O, (2, 1) -> O, (2, 2) -> X)
  val t3 = Map((0, 0) -> X, (0, 1) -> X, (0, 2) -> X, (1, 0) -> O, (1, 1) -> O, (1, 2) -> X, (2, 0) -> O, (2, 1) -> X, (2, 2) -> O)
  val t4 = Map((0, 0) -> O, (0, 1) -> X, (0, 2) -> X, (1, 0) -> X, (1, 1) -> O, (1, 2) -> X, (2, 0) -> X, (2, 1) -> X, (2, 2) -> O)
  val t5 = Map((0, 0) -> X, (0, 1) -> O, (0, 2) -> O, (1, 0) -> O, (1, 1) -> X, (1, 2) -> X, (2, 0) -> O, (2, 1) -> O)
  val t6 = Map((0, 0) -> O, (0, 2) -> O, (1, 0) -> O, (1, 1) -> O, (1, 2) -> X, (1, 3) -> X, (2, 0) -> O, (2, 1) -> X, (2, 2) -> O, (2, 3) -> O, (3, 0) -> X, (3, 1) -> O, (3, 2) -> X, (3, 3) -> X)
  val t7 = Map((0, 0) -> X, (0, 1) -> X, (0, 2) -> X, (0, 3) -> X, (1, 0) -> O, (1, 1) -> O, (1, 2) -> X, (1, 3) -> X, (2, 0) -> O, (2, 1) -> X, (2, 2) -> O, (2, 3) -> X, (3, 0) -> X, (3, 1) -> O, (3, 2) -> X, (3, 3) -> X)
  val t8 = Map((0, 0) -> X, (0, 1) -> O, (2, 0) -> X, (2, 1) -> O)

  test("test1"){
    assert(minimax(createGame(X,3,t1)) == Some(O))
  }

  test("test2"){
    assert(minimax(createGame(X,3,t2)) == Some(O))
  }

  test("test3"){
    assert(minimax(createGame(X,3,t3)) == Some(X))
  }

  test("test4"){
    assert(minimax(createGame(X,3,t4)) == Some(O))
  }

  test("test5"){
    assert(minimax(createGame(X,3,t5)) == Some(O))
  }
/*
  test("test6"){
    assert(minimax(createGame(X,4,t6)) == Some(X))
  }
*/
  test("test7"){
    assert(minimax(createGame(X,4,t7)) == Some(X))
  }
/*
  test("test8"){
    assert(minimax(createGame(X,4,t8)) == Some(O))
  }
*/
  test("test9"){
    assert(minimax(createGame(O,3,t1)) == Some(X))
  }

  test("test10"){
    assert(minimax(createGame(O,3,t2)) == Some(O))
  }

  test("test11"){
    assert(minimax(createGame(O,3,t3)) == Some(X))
  }

  test("test12"){
    assert(minimax(createGame(O,3,t4)) == Some(O))
  }

  test("test13"){
    assert(minimax(createGame(O,3,t5)) == Some(X))
  }
/*
  test("test14"){
    assert(minimax(createGame(O,4,t6)) == Some(X))
  }

  test("test15"){
    assert(minimax(createGame(O,4,t7)) == Some(X))
  }

  test("test16"){
    assert(minimax(createGame(O,4,t8)) == Some(X))
  }
*/
  test("test isFinish"){
    assert(createGame(X,3,t2).isFinished() == true)
    assert(createGame(X,3,t8).isFinished() == false)
  }

  test("test winner"){
    assert(createGame(X,3,t2).getWinner() == Some(O))
    assert(createGame(X,3,t8).getWinner() == None)
  }

  test("test nextBoards"){
    assert(createGame(X,3,t8).nextBoards().size == 5)
    assert(createGame(X,3,t1).nextBoards().size == 1)
  }

}
