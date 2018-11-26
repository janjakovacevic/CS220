object Lists {

  val oddNumbers = 1 :: 3 :: 5 :: Nil

  def sumDouble(alist: List[Int]): Int = alist match{
      case Nil => 0
      case head :: tail => 2*head + sumDouble(tail)
    }

  def removeZeroes(alist: List[Int]): List[Int] = alist match{
      case Nil => Nil
      case 0 :: tail => removeZeroes(tail)
      case head :: tail => head :: removeZeroes(tail)
    }

   def countEvens(alist: List[Int]): Int = alist match{
      case Nil => 0
      case head :: tail => (head % 2 == 0) match{
        case true => 1 + countEvens(tail)
        case false => countEvens(tail)
      }
    }

  def removeAlternating(alist: List[String]): List[String] = alist match{
      case Nil => Nil
      case head :: tail => tail match {
        case Nil => head :: Nil
        case head1 :: tail1 => head :: removeAlternating(tail1)
     }
   }

 def isAscending(alist: List[Int]): Boolean = alist match{
      case Nil => true
      case head :: tail => tail match {
        case Nil => true
        case head1 :: tail1 => (head <= head1) match{
          case true => isAscending(tail)
          case false => false
          }
        }
    }

  def addSub(alist: List[Int]): Int = alist match{
      case Nil => 0
      case head :: tail => tail match {
        case Nil => head
        case head1 :: tail1 => tail1 match {
          case Nil => head - head1
          case head2 :: tail2 => head - head1 + head2 - addSub(tail2)
          }
      }
    }

  def alternate(alist: List[Int], blist: List[Int]): List[Int] = (alist, blist) match{ //should assume same length
    case (Nil, Nil) => Nil
    case (head1 :: tail1, head2 :: tail2) => head1 :: head2 :: alternate(tail1, tail2)
    case _ => throw new Exception("Invalid Argument")
    }
  
  def fromTo(a: Int, b: Int): List[Int] = (a == b) match{ //should assume a < b
    case true => Nil
    case false => (a < b) match{
      case true => a :: fromTo(a + 1, b)
      case false => throw new Exception("Invalid Argument")
      } 
    }

  def insertOrdered(n: Int, alist: List[Int]): List[Int] = alist match{
    case Nil => n :: Nil
    case head :: tail => (n <= head) match{
      case true => n :: head :: tail
      case false => head :: insertOrdered(n, tail)
      }
    }

 def sort(alist: List[Int]): List[Int] = alist match{
    case Nil => Nil
    case head :: tail => insertOrdered(head, sort(tail))
    }

  def allLT(alist: List[Int], n: Int): List[Int] = alist match{
    case Nil => Nil
    case head :: tail => (head < n) match{
      case true => head :: allLT(tail, n)
      case false => allLT(tail, n)
      }
    }

  def allGTE(alist: List[Int], n: Int): List[Int] = alist match{
    case Nil => Nil
    case head :: tail => (head >= n) match{
      case true => head :: allGTE(tail, n)
      case false => allGTE(tail, n)
      }
    }

  def quickSort(alist: List[Int]): List[Int] = alist match{
    case Nil => Nil
    case head :: tail => quickSort(allLT(tail, head)) ::: List(head) ::: quickSort(allGTE(tail, head))
    }

}