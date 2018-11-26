class Tests extends org.scalatest.FunSuite {

  import Main._
  import hw.streams.Generator  

  test("Implemented the Solution interface") {
    val main: hw.streams.SolutionLike = Main
  }

  test("from test") {
    assert(nth(from(10), 10) == 20)
    assert(nth(from(0), 0) == 0)
    assert(nth(from(0), 5) == 5)
    }

  val num: Generator[Int] = map((x: Int) => x*2, from(0))
  val power: Generator[Double] = map((x: Int) => math.pow(2, x), from(0))
  val gen1: Generator[Int] = from(0)
  val gen2: Generator[Int] = from(2)

  def fromDouble(x: Double): Generator[Double] = cons(x, fromDouble(x + 1))
  val gen3: Generator[Double] = fromDouble(0)
  val gen4: Generator[Double] = fromDouble(4)

  def isEven(x: Int): Boolean = {
    if(x % 2 == 0) true
    else false
    }

  test("map test") {
    assert(nth(num, 0) == 0)
    assert(nth(num, 1) == 2)
    assert(nth(num, 2) == 4)
    assert(nth(num, 3) == 6)
    assert(nth(num, 100) == 200)
    }

  test("pow test") {
    assert(nth(power, 0) == 1)
    assert(nth(power, 4) == 16)
    assert(nth(power, 10) == 1024)
    }

  test("filter test") {
    val one = filter((x: Int) => x == 2, from(0))
    val two = filter((x: Int) => x == 3, from(3))
    assert(nth(filter(isEven, from(2)), 4) == 10)
    }
  
  test("interleave test") {
    assert(nth(interleave(gen1, gen2), 1) == 2)
    assert(nth(interleave(gen1, gen2), 3) == 3)
    }

  test("sift test") {
    assert(nth(sift(2, gen1), 0) == 1)
    assert(nth(sift(2, gen1), 3) == 7)
    assert(nth(sift(5, gen1), 4) == 6)
    }
  
  test("prime test") {
    assert(nth(prime, 0) == 2)
    assert(nth(prime, 1) == 3)
    assert(nth(prime, 2) == 5)
    assert(nth(prime, 3) == 7)
    assert(nth(prime, 4) == 11)
    assert(nth(prime, 5) == 13)
    assert(nth(prime, 6) == 17)
    assert(nth(prime, 7) == 19)
    assert(nth(prime, 8) == 23)
    assert(nth(prime, 9) == 29)
    assert(nth(prime, 10) == 31)
    assert(nth(prime, 999) == 7919)
    assert(nth(prime, 1000) == 7927)
    }
  
  test("total test") {
    assert(nth(total(gen3), 3) == 6)
    assert(nth(total(gen3), 0) == 0)
    assert(nth(total(gen3), 5) == 15)
    assert(nth(total(gen4), 0) == 4)
    assert(nth(total(gen4), 2) == 15)
    assert(nth(total(gen4), 4) == 30)
    }

}
