  import hw.streams.Generator

object Main extends hw.streams.SolutionLike { // do not change this line

  // This helper function is very useful
  def cons[A](head: A, tail: => Generator[A]): Generator[A] = new Generator[A] {
    def next() = (head, tail) //backwards version of next()
  }

  // The example from the project description
  val ones: Generator[Int] = cons(1, ones)

  def from(x: Int): Generator[Int] = cons(x, from(x + 1))

  def map[A,B](f: A => B, agen: Generator[A]): Generator[B] = 
    cons(f(agen.next()._1), map(f, agen.next()._2))

  val pow: Generator[Int] = map((x: Int) => math.pow(2, x).toInt, from(0))

  def nth[A](agen: Generator[A], index: Int): A = {
    if(index == 0){
      val (x, _) = agen.next()
      x
    }
    else{
      val (_, x) = agen.next()
      nth(x, index - 1)
      } 
    }

  def filter[A](pred: (A) => Boolean, agen: Generator[A]): Generator[A] = {
    val (head, tail) = agen.next()
    if(pred(head)) cons(head, filter(pred, tail))
    else filter(pred, tail)
    }

  def interleave[A](agen1: Generator[A], agen2: Generator[A]): Generator[A] = {
    val (head, tail) = agen1.next()
    cons(head, interleave(agen2, tail))
    }

  def sift(n: Int, agen: Generator[Int]): Generator[Int] = {
    val (head, tail) = agen.next()
    if(head % n == 0) sift(n, tail)
    else cons(head, sift(n, tail)) 
    }

  def nextPrime(agen: Generator[Int]): Generator[Int] = {
    val (head, tail) = agen.next()
    cons(head, nextPrime(sift(head, tail)))
    }

  val prime: Generator[Int] = {
    nextPrime(from(2))
    }

  def total(agen: Generator[Double]): Generator[Double] = {
    val (head1, tail1) = agen.next()
    val (head2, tail2) = tail1.next()
    cons(head1, total(cons(head1 + head2, tail2)))
    }
}