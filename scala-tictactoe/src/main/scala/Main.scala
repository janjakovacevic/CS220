import hw.tictactoe._

class Game(turn: Player, dim: Int, board: Map[(Int, Int), 
  Player]) extends GameLike[Game] {

  def isFinished(): Boolean = {
    if(board.size == dim*dim) true
    else if (getWinner() == Some(X) || getWinner() == Some(O)) true
    else false
    }

  def one: Player = turn

  def other: Player = {
    if(one == X) O else X
    }

  def checkBox(tuple: (Int, Int)): Option[Player] = {
    if(board.get(tuple) == Some(O)) Some(O)
    else if(board.get(tuple) == Some(X)) Some(X)
    else None
    }

  def checkList(list: List[Option[Player]]): Option[Player] = list match {
    case Nil => list(0)
    case head :: Nil => list(0)
    case head :: tail => if(head == tail(0)) checkList(tail) else None
    }

  def rowHelp(tuple: (Int, Int)): List[Option[Player]] = {
    if(tuple._2 < dim) 
      checkBox(tuple._1, tuple._2) :: rowHelp(tuple._1, tuple._2 + 1)
    else Nil
    }

  def row(tuple: (Int,Int)): List[Option[Player]] = {
    if(tuple._1 < dim) 
      checkList(rowHelp(tuple)) :: row(tuple._1 + 1, tuple._2)
    else Nil
    }

  def columnHelp(tuple: (Int, Int)): List[Option[Player]] = {
    if(tuple._1 < dim) 
      checkBox(tuple._1, tuple._2) :: columnHelp(tuple._1 + 1, tuple._2)
    else Nil
    }

  def column(tuple: (Int, Int)): List[Option[Player]] = {
    if(tuple._2 < dim)
      checkList(columnHelp(tuple)) :: column(tuple._1, tuple._2 + 1)
    else Nil
    }

  def crossHelp1(tuple: (Int, Int)): List[Option[Player]] = {
    if(tuple._1 < dim && tuple._2 < dim)
      checkBox(tuple._1, tuple._2) :: crossHelp1(tuple._1 + 1, tuple._2 + 1)
    else Nil
    }

  def crossHelp2(tuple: (Int, Int)): List[Option[Player]] = {
    if(tuple._1 < dim && tuple._2 >= 0)
      checkBox(tuple._1, tuple._2) :: crossHelp2(tuple._1 + 1, tuple._2 - 1)
    else Nil
    }

  def cross(tuple1: (Int, Int), tuple2: (Int, Int)): List[Option[Player]] = {
      List(checkList(crossHelp1(tuple1)), checkList(crossHelp2(tuple2)))
    }

  def xWins: Boolean = {
    if(row(0, 0).contains(Some(X)) || 
      column(0, 0).contains(Some(X)) ||
      cross((0, 0), (0, dim - 1)).contains(Some(X))) true
    else false
    }

  def oWins: Boolean = {
     if(row(0, 0).contains(Some(O)) || 
      column(0, 0).contains(Some(O)) ||
      cross((0, 0), (0, dim - 1)).contains(Some(O))) true
    else false
    }

  def getWinner(): Option[Player] = {
    //only called directly after isFinished()
    if(xWins) Some(X)
    else if(oWins) Some(O)
    else None
    }

  def emptyBox(board: Map[(Int, Int), Player], row: Int, col: Int): List[(Int, Int)] = {
    if(row < dim && col < dim){
      if(board.get(row, col) == None) List((row, col)) ::: emptyBox(board, row + 1, col) ::: emptyBox(board, row, col + 1)
      else emptyBox(board, row + 1, col) ::: emptyBox(board, row, col + 1)
      }
    else Nil
    }

/*
  def columnMove(tuple: (Int, Int)): List[(Int, Int)] = {
    if(tuple._2 < dim) {
      (tuple._1, tuple._2) :: columnMove((tuple._1, tuple._2 + 1))
      }
    else Nil
    }

  def move(tuple: (Int, Int)): List[(Int, Int)] = {
    if(tuple._1 < dim){
      columnMove(tuple) ::: move((tuple._1 + 1, tuple._2))
      }
    else Nil
    }

  def checkOverlaps(list1: List[(Int, Int)], list2: List[(Int, Int)]): List[(Int, Int)] = 
    list1 match {
      case Nil => Nil
      case head :: tail => if(!list2.contains(head)) head :: checkOverlaps(tail, list2)
                           else checkOverlaps(tail, list2)
      }
*/

  def helpNextBoard(list: List[(Int, Int)]): List[Game] = list match {
    case Nil => Nil
    case head :: tail => 
      if(one == X) new Game(O, dim, board.updated(head, O)) :: helpNextBoard(tail)
      else new Game(X, dim, board.updated(head, X)) :: helpNextBoard(tail)
    }

  def nextBoards(): List[Game] = {
    helpNextBoard(emptyBox(board, 0, 0).distinct) //distinct ensure there are no duplicates
  //  val list = checkOverlaps(move(0,0), board.keySet.toList)
  //  helpNextBoard(list)
    }
}

object Solution extends MinimaxLike {
  type T = Game // T is an "abstract type member" of MinimaxLike

  def createGame(turn: Player, dim: Int, board: 
    Map[(Int, Int), Player]): Game = {
      new Game(turn, dim, board)
      }

  def minimax(board: Game): Option[Player] = {
    if(board.isFinished()){
      board.getWinner()
      }
    else{
     val next = board.nextBoards.toStream.map(b => minimax(b)) /*.toStream. to terminate early; before map*/ 
     //who won given all these boards
      if(next.contains(Some(board.one)))
        Some(board.one)
     else if(next.contains(None))
        None
      else 
        Some(board.other)
      }
  }
}
