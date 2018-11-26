import hw.sudoku._

object Solution extends SudokuLike {
  type T = Board

  def parse(str: String): Board = parseHelper(str.toList.zip(allPositions))

  def calculateAllPositions(x: Int): List[(Int, Int)] = {
    if(x == 81) Nil
    else (x/9, x % 9) :: calculateAllPositions(x + 1)
    }
 
  val allPositions = calculateAllPositions(0)
  val oneTo9 = 1.to(9).toList
  val emptyBoard = new Board(allPositions.map(x => x -> oneTo9).toMap)

  def parseHelper(list: List[(Char, (Int, Int))]): Board = list match {
    case Nil => emptyBoard
    case ('.', _) :: rest => parseHelper(rest)
    case (digit,(row, col)) :: rest => {
      val n = digit.toString.toInt
      parseHelper(rest).place(row, col, n)
      }
    }

  // You can use a Set instead of a List (or, any Iterable)
  def peers(row: Int, col: Int): List[(Int, Int)] = peersTable((row, col))

  val peersTable = Map((0.to(8).flatMap{
    r => 0.to(8).map{
      c => ((r, c) -> calculatePeers(r, c))
      }
    }) :_*)

  def calculatePeers(row: Int, col: Int): List[(Int, Int)] = {
    val rowPeers = 0.to(8).map{r => (r, col)}
    val colPeers = 0.to(8).map{c => (row, c)}
    val boxRow = (row/3)*3
    val boxCol = (col/3)*3
    val boxPeers = boxRow.to(boxRow + 2).flatMap{
      r => boxCol.to(boxCol + 2).map{
        c => (r, c)
        }
      }
    (rowPeers ++ colPeers ++ boxPeers).filterNot{
      case(r, c) => r == row && c == col
      }.toList.distinct
    }
}

// Top-left corner is (0,0). Bottom-right corner is (8,8). Feel free to
// change the fields of this class.
class Board(val available: Map[(Int, Int), List[Int]]) extends BoardLike[Board]{
  
  import Solution._

  def availableValuesAt(row: Int, col: Int): List[Int] = {
    // Assumes that a missing value means all values are available. Feel
    // free to change this.
    available.getOrElse((row, col), 1.to(9).toList)
  }

  def valueAt(row: Int, col: Int): Option[Int] = {
    if(available((row, col)).size == 1) Option(available((row, col)).head)
    else None
    }

  def isSolved(): Boolean = available.forall(x => x._2.size == 1)

  def isUnsolvable(): Boolean = available.exists(x => x._2.size == 0)

  def place(row: Int, col: Int, num: Int): Board = {
			require(availableValuesAt(row, col).contains(num))
			new Board(placeHelper(num, peers(row, col), 
        available+((row, col) -> List(num))))
		}

  def placeHelper(num: Int, list: List[(Int, Int)], board: Map[(Int, Int), 
    List[Int]]): Map[(Int, Int), List[Int]] = list match{
      case Nil => board
      case h :: t =>
        if(board(h._1, h._2).contains(num)) {
          if((board+((h._1, h._2) -> (board(h._1, h._2).filterNot(x => 
            x == num))))(h._1, h._2).size == 1){
            placeHelper(num, t, placeHelper((board+((h._1, h._2) -> 
              (board(h._1, h._2).filterNot(x => x == num))))(h._1, h._2).head, 
              peers(h._1, h._2), board+((h._1, h._2) -> 
              board(h._1, h._2).filterNot(x => x == num))))
          }
          else placeHelper(num, t, board+((h._1, h._2) -> 
            board(h._1, h._2).filterNot(x => x == num)))
        }
        else placeHelper(num, t, board)
    }

//using Streams
 	def nextStates(): Stream[Board] = {
      if(isUnsolvable()) Stream[Board]()
      else available.foldLeft(Stream[Board]())((a, b) => 
        if(available(b._1).size == 1) a
        else a ++: available(b._1).foldLeft(Stream[Board]())((c, d) => 
          c :+ place(b._1._1, b._1._2, d))).sortBy(a => 
          a.available.foldLeft(0)((c, d) => c + a.available(d._1).size))     
		}

  def solve(): Option[Board] = {
			if(isSolved()) Some(this)
			else solveHelper(nextStates())
		}

  def solveHelper(stream: Stream[Board]): Option[Board] = stream match{
      case h #:: t => {
        if(h.solve() != None) h.solve()
        else solveHelper(t)
      }
      case _ => None
  }
}