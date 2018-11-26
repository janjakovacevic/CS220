import HOF._

// You will need to write more tests.
class TestSuite extends org.scalatest.FunSuite {

  test("map2 test") {
    def add(x: Int, y: Int): Int = x + y
    def product(x: Int, y: Int): Int = x*y
    assert(map2(add, List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
    assert(map2(add, List(-1,4), List(2,-3)) == List(1,1))
    assert(map2(product, List(1, 2, 3), List(4, 5, 6)) == List(4, 10, 18))
    assert(map2(product, List(-9,2), List(-2,9)) == List(18,18))
    assertThrows[Exception]{map2(add, List(1), Nil)}
    assertThrows[Exception]{map2(product, List(1,2), List(0))}
  } 

  test("zip test") {
    assert(zip(List(1, 2, 3), List(4, 5, 6)) == List((1,4), (2, 5), (3, 6)))
    assert(zip(List("George", "Teddy"), List("Washington", "Roosevelt")) ==
          List(("George", "Washington"), ("Teddy", "Roosevelt")))
    assert(zip(List(-1,2,-3), List(1,-2,3)) == List((-1,1), (2,-2), (-3,3)))
    assertThrows[Exception]{zip(List(1), Nil)}
    assertThrows[Exception]{zip(List(1,2,3), List(1,2,3,4))}
    assertThrows[Exception]{zip(List(), List(9,8,7))}
  }
  test("flatten test") {
    assert(flatten(List(List(1, 2), List(3, 4))) == List(1, 2, 3, 4))
    assert(flatten(List(List(1,2), List(), List(3,4))) == List(1,2,3,4))
    assert(flatten(List(Nil, Nil, Nil)) == Nil)
    assert(flatten(List(Nil, List(5, 6, 7), Nil)) == List(5, 6, 7))
  }

  test("flatten3 test") {
    assert(flatten3(List(List(List(1,2), List(3,4), List(5,6)))) == List(1,2,3,4,5,6))
    assert(flatten3(List(List(List(), List(4,5), List(6,7)))) == List(4,5,6,7))
    assert(flatten3(List(List(List()))) == Nil)
    assert(flatten3(List(List(List(-5,7,4), List(5,-7), List(1)))) == List(-5,7,4,5,-7,1))
    }

  test("buildList test") {
    def f(x: Int) = x
    def g(x: Int) = x*x
    def h(x: Int) = x+1
    assert(buildList(10, f) == List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    assert(buildList(4, g) == List(0, 1, 4, 9))
    assert(buildList(8, h) == List(1, 2, 3, 4, 5, 6, 7, 8))
    assert(buildList(0, f) == List())
    assertThrows[Exception]{(buildList(-4,g))}
  }

  test("mapList test") {
    def f(n: Int): List[Int] = buildList(n, (_: Int) => n)
    def g(n: Int): List[Int] = buildList(n, (_: Int) => 2*n)
    def h(n: Int): List[Int] = buildList(n, (_: Int) => n*n)
    assert(mapList(List(1, 2, 3), f) == List(1, 2, 2, 3, 3, 3))
    assert(mapList(List(0, 1, 3, 5), f) == List(1, 3, 3, 3, 5, 5, 5, 5, 5))
    assert(mapList(List(1, 2), g) == List(2,4,4))
    assert(mapList(List(4, 5), h) == List(16,16,16,16,25,25,25,25,25))
  }

  test("partition test") {
  def isOdd(n: Int): Boolean = n % 2 == 1
  def isEven(n: Int): Boolean = n % 2 == 0

    assert(partition(isOdd, List(1)) == (List(1), List()))
    assert(partition(isOdd, List(2)) == (List(), List(2)))
    assert(partition(isOdd, List(1,2,3,4,5,6)) == (List(1, 3, 5), List(2, 4, 6)))
    assert(partition(isOdd, List(2,4,6)) == (Nil, List(2,4,6)))
    assert(partition(isOdd, List(1,3,5)) == (List(1,3,5), Nil))
    assert(partition(isOdd, List()) == (Nil, Nil))

    assert(partition(isEven, List(1)) == (List(), List(1)))
    assert(partition(isEven, List(2)) == (List(2), List()))
    assert(partition(isEven, List(1,2,3,4,5,6)) == (List(2,4,6), List(1,3,5)))
    assert(partition(isEven, List(2,4,6)) == (List(2,4,6), Nil))
    assert(partition(isEven, List(1,3,5)) == (Nil, List(1,3,5)))
    assert(partition(isEven, List()) == (Nil, Nil))
  } 

  def lt(x: Int, y: Int): Boolean = x < y
  def gt(x: Int, y: Int): Boolean = x > y

  test("merge test") {
    assert(merge(lt, List(1,3,5), List(0,6,10)) == List(0,1,3,5,6,10))
    assert(merge(gt, List(1,3,5), List(0,6,10)) == List(10,6,5,3,1,0))
    assert(merge(lt, List(-3,-9,8), List(3,7,-10)) == List(-10,-9,-3,3,7,8))
    assert(merge(gt, List(-3,-9,8), List(3,7,-10)) == List(8,7,3,-3,-9,-10))
    assert(merge(lt, List(1,2,-3), List(3,4)) == List(-3,1,2,3,4))
    assert(merge(gt, List(4,6), List(-1,-8,10)) == List(10,6,4,-1,-8))
    assert(merge(lt, List(), List(2,2,1)) == List(1,2,2))
    assert(merge(gt, List(1,7), List()) == List(7,1))
  }

  test("sort test") {
    assert(sort(lt, List(5,1,2,3,4,5)) == List(1,2,3,4,5,5))
    assert(sort(gt, List(5,1,2,3,4,5)) == List(5,5,4,3,2,1))
    assert(sort(lt, List(-5,-6,-9,-23,-1)) == List(-23,-9,-6,-5,-1))
    assert(sort(gt, List(-5,-6,-9,-23,-1)) == List(-1,-5,-6,-9,-23))
    assert(sort(lt, Nil) == Nil)
    assert(sort(gt, Nil) == Nil)
  }

  test("concat test") {
    assert(helperConcat(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))
    assert(helperConcat(List(1), List(2,3,4)) == List(1,2,3,4))
    assert(helperConcat(List(), List(1,2,3)) == List(1,2,3))
    assert(helperConcat(List(1,2), List()) == List(1,2))
    assert(helperConcat(List(1,2,3,4), List(5,6)) == List(1,2,3,4,5,6))
    }

}