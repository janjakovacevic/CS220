import Lists._

class TestSuite extends org.scalatest.FunSuite {

  test("oddNumbers properly defined") {
    assert(oddNumbers == List(1, 3, 5))
  }

  test("test sumDouble") {
    assert(sumDouble(List(1, 2, 3, 4, 5)) == 30)
    assert(sumDouble(List(24, 11, 9)) == 88)
    assert(sumDouble(List(-1, -2, -3, 6)) == 0)
    assert(sumDouble(List(-10, -20, -100)) == -260)
    assert(sumDouble(List(0)) == 0)
    assert(sumDouble(List()) == 0)
    }

  test("test removeZeroes") {
    assert(removeZeroes(List(0, 4, 9, 5, 8, 0, 0, 0)) == List(4, 9, 5, 8))
    assert(removeZeroes(List(1, 3, 5, 6, 9)) == List(1, 3, 5, 6, 9))
    assert(removeZeroes(List(-1, -2, -4, 0, 0)) == List(-1, -2, -4))
    assert(removeZeroes(List(0, 0, 0, 0, 0)) == Nil)
    assert(removeZeroes(Nil) == Nil)
    }

  test("test countEvens") {
    assert(countEvens(List(0, 2, 4, 6, 7)) == 4)
    assert(countEvens(List(3, 5, 7, 7, 1)) == 0)
    assert(countEvens(List(-1, -2, 7, -6, -8)) == 3)
    assert(countEvens(List(0)) == 1)
    assert(countEvens(List()) == 0)
    assert(countEvens(List(-2, 3, 7, 8, 10, -12)) == 4)
    }

  test("test removeAlternating") {
    assert(removeAlternating(List("mum", "tree", "dad", "floor", "family", "bed", "love")) == List("mum", "dad", "family", "love"))
    assert(removeAlternating(List("umass")) == List("umass"))
    assert(removeAlternating(Nil) == Nil)
    assert(removeAlternating(List("A", "B")) == List("A"))
    }

  test("test isAscending") {
    assert(isAscending(List(0, 1, 2, 3, 3, 4, 5)) == true)
    assert(isAscending(List(4, 5, -9, 0, 3)) == false)
    assert(isAscending(List(-4, -3, -2, -1)) == true)
    assert(isAscending(Nil) == true)
    assert(isAscending(List(1, 1, 1, 1, 1)) == true)
    assert(isAscending(List(1, -2, 3)) == false)
  }

  test("test addSub") {
    assert(addSub(List(10, 20, 30, 40, 50, 60, 70, 80)) == -40)
    assert(addSub(List(-10, 1, -30, 100)) == -141)
    assert(addSub(List(-4, -3, -8, -9, -23)) == -23)
    assert(addSub(Nil) == 0)
    assert(addSub(List(100, 90, 20)) == 30)
    }

  test("test alternate") {
    assert(alternate(List(1, 3, 5), List(2, 4, 6)) == List(1, 2, 3, 4, 5, 6))
    assert(alternate(List(4, 7), List(1, -2)) == List(4, 1, 7, -2))
    assert(alternate(Nil, Nil) == Nil)
    assert(alternate(List(-40), List(10)) == List(-40, 10))
    assertThrows[Exception]{alternate(List(2, 3), Nil)}
    }

  test("test fromTo") {
    assert(fromTo(0, 3) == List(0, 1 ,2))
    assert(fromTo(0, 1) == List(0))
    assert(fromTo(14, 20) == List(14, 15, 16, 17, 18, 19))
    assert(fromTo(3, 10) == List(3, 4, 5, 6, 7, 8, 9))
    assert(fromTo(-2, 3) == List(-2, -1, 0, 1, 2))
    assert(fromTo(-6,-2) == List(-6, -5, -4, -3))
    assertThrows[Exception] {fromTo(5, 0)} 
    }

  test("test insertOrdered") {
    assert(insertOrdered(3, List(1, 2, 4, 5)) == List(1, 2, 3, 4, 5))
    assert(insertOrdered(10, List(20, 30, 40)) == List(10, 20, 30, 40))
    assert(insertOrdered(0, Nil) == List(0))
    assert(insertOrdered(5, List(0, 5, 10, 15)) == List(0, 5, 5, 10, 15))
    assert(insertOrdered(-1, List(-3, 0, 1, 3)) == List(-3, -1, 0, 1, 3))
    assert(insertOrdered(-10, List(0, 10, 20, 30)) == List(-10, 0, 10, 20, 30))
    }

  test("test sort") {
    assert(sort(List(1, 2, 4, 3)) == List(1, 2, 3, 4))
    assert(sort(List(1)) == List(1))
    assert(sort(Nil) == Nil)
    assert(sort(List(9, 8, 7, 6, 5)) == List(5, 6, 7, 8, 9))
    assert(sort(List(-6, -30, 9, -29, 8)) == List(-30, -29, -6, 8 ,9))
    }

  test("test allLT") {
    assert(allLT(List(1, 4, 7, 10), 7) == List(1, 4))
    assert(allLT(List(10, 20, 30), 35) == List(10, 20, 30))
    assert(allLT(List(7, 14), 5) == Nil)
    assert(allLT(Nil, 10) == Nil)
    assert(allLT(List(-20, -5, -29), -1) == List(-20, -5, -29))
    }

  test("test allGTE") {
    assert(allGTE(List(1, 4, 7, 10), 7) == List(7, 10))
    assert(allGTE(List(10, 20, 30), 35) == Nil)
    assert(allGTE(List(7, 14), 5) == List(7, 14))
    assert(allGTE(Nil, 10) == Nil)
    assert(allGTE(List(-20, -5, -29), -1) == Nil)
    }

  test("test quickSort") { //not any different test from sort test??
    assert(quickSort(List(1, 2, 4, 3)) == List(1, 2, 3, 4))
    assert(quickSort(List(1)) == List(1))
    assert(quickSort(Nil) == Nil)
    assert(quickSort(List(9, 8, 7, 6, 5)) == List(5, 6, 7, 8, 9))
    assert(quickSort(List(-5, -9, -6, -10)) == List(-10, -9, -6, -5))
    }

}