object HOF {

  def map2[A,B,C](f: (A, B) => C, lst1: List[A], lst2: List[B]): List[C] = (lst1, lst2) match{
    case (Nil, Nil) => Nil
    case (head1 :: tail1, head2 :: tail2) => f(head1, head2) :: map2[A, B, C](f, tail1, tail2)
    case _ => throw new Exception("different lengths")
    }

  def zip[A,B](lst1: List[A], lst2: List[B]): List[(A, B)] = (lst1, lst2) match{
    case (Nil, Nil) => Nil
    case (head1 :: tail1, head2 :: tail2) => (head1, head2) :: zip[A,B](tail1, tail2)
    case _ => throw new Exception("different lengths")
    }

//helper method for concatenation (used for flattens and mapList)
  def helperConcat[A](a: List[A], b: List[A]): List[A] = a match{
    case Nil => b
    case head :: tail => head :: helperConcat(tail, b)
    }

  def flatten[A](lst: List[List[A]]): List[A] = lst match{
    case Nil => Nil
    case head :: tail => helperConcat(head, flatten(tail))
    }

  def flatten3[A](lst: List[List[List[A]]]): List[A] = lst match{
    case Nil => Nil
    case head :: tail => head match{
      case Nil => flatten3(tail)
      case h :: t => helperConcat(h, flatten(t))
      }
    }

  def buildList[A](length: Int, f: Int => A): List[A] = length match {
    case 0 => Nil
    case length: Int if(length < 0) => throw new Exception("negative length")
    case _ => helperConcat(buildList(length - 1, f), f(length-1) :: Nil)
  }

  def mapList[A, B](lst: List[A], f: A => List[B]): List[B] = lst match{
    case Nil => Nil
    case head :: tail => helperConcat(f(head), mapList(tail, f))
    }

//helper method for partition (used for partition only) - handles true
  def helpPartTrue[A](f: A => Boolean, a: List[A]): List[A] = a match {
    case Nil => Nil
     case head :: tail => tail match {
      case Nil => f(head) match {
         case true => List(head)
         case false => Nil
         }
      case h :: t => f(head) match {
         case true => head :: helpPartTrue(f, tail)
         case false => helpPartTrue(f, tail)
        }
       }
     }

//helper method for partition (used for partition only) - handles false
 def helpPartFalse[A](f: A => Boolean, a: List[A]): List[A] = a match {
    case Nil => Nil
     case head :: tail => tail match {
      case Nil => f(head) match {
         case true => Nil
         case false => List(head)
         }
      case h :: t => f(head) match {
         case true => helpPartFalse(f, tail)
         case false => head :: helpPartFalse(f, tail)
        }
       }
     }

  def partition[A](f: A => Boolean, lst: List[A]): (List[A], List[A]) = lst match{
    case Nil => (Nil, Nil)
    case head :: tail => (helpPartTrue(f, lst), helpPartFalse(f, lst))
    }

//helper method for insertion (used for sort and merge)
 def helperInsert[A](lessThan: (A, A) => Boolean, x: A, alist: List[A]): List[A] = alist match{
    case Nil => List(x)
    case head :: tail => if(lessThan(x, head)) x :: head :: tail else head :: helperInsert(lessThan, x, tail)
    }

  def merge[A](lessThan: (A, A) => Boolean, alist1: List[A], alist2: List[A]): List[A] = (alist1, alist2) match{
    case (Nil, Nil) => Nil
    case (Nil, head1 :: tail1) => sort(lessThan, head1 :: merge(lessThan, Nil, tail1))
    case (head1 :: tail1, Nil) => sort(lessThan, head1 :: merge(lessThan, tail1, Nil))
    case (head1 :: tail1, head2 :: tail2) => sort(lessThan, head1 :: head2 :: merge(lessThan, tail1, tail2))
    }

  def sort[A](lessThan: (A, A) => Boolean, alist: List[A]): List[A] = alist match{
    case Nil => Nil
    case head :: tail => helperInsert(lessThan, head, sort(lessThan, tail))
    }

}