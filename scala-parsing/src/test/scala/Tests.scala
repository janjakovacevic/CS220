import ArithEval._
import ArithParser._
import ArithPrinter._
import hw.parsing._

import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object GenRegex {

  def genNum: Gen[Double] = Gen.oneOf(2, 3, 4)

  def genNums: Gen[Expr] = for {
    x <- genNum
  } yield (Num(x))

  def genAdd(size: Int): Gen[Expr] = for {
    x <- genRegex(size / 2)
    y <- genRegex(size / 2)
  } yield (Add(x, y))

  def genSub(size: Int): Gen[Expr] = for {
    x <- genRegex(size / 2)
    y <- genRegex(size / 2)
  } yield (Sub(x, y))

  def genMul(size: Int): Gen[Expr] = for {
    x <- genRegex(size / 2)
    y <- genRegex(size / 2)
  } yield (Mul(x, y))

  def genDiv(size: Int): Gen[Expr] = for {
    x <- genRegex(size / 2)
     y <- genRegex(size / 2)
  } yield (Div(x, y))

  def genExp(size: Int): Gen[Expr] = for {
    x <- genRegex(size / 2)
    y <- genRegex(size / 2)
  } yield (Exponent(x, y))

  def genRegex(size: Int): Gen[Expr] = {
    if (size <= 0) genNums
    else Gen.oneOf(genAdd(size), genSub(size), genMul(size), genDiv(size), genExp(size), genNums)
  }

  def regex: Gen[Expr] = Gen.sized(genRegex)
}

class TestSuite extends org.scalatest.FunSuite with GeneratorDrivenPropertyChecks {

    test("several objects must be defined") {
    val parser: hw.parsing.ArithParserLike = ArithParser
    val printer: hw.parsing.ArithPrinterLike = ArithPrinter
    val eval: hw.parsing.ArithEvalLike = ArithEval
  }

  def check1(str: String) = {
    val re1 = ArithParser.parseAll(ArithParser.expr, str).get
    val re2 = ArithParser.parseAll(ArithParser.expr, ArithPrinter.print(re1)).get
    val re3 = ArithParser.parseAll(ArithParser.mul, str).get
    val re4 = ArithParser.parseAll(ArithParser.mul, ArithPrinter.print(re3)).get
    assert(re1 == re2)
    assert(re3 == re4)
  }

 def check2(re1: Expr) = {
    val re2 = ArithParser.parseAll(ArithParser.expr, ArithPrinter.print(re1)).get
    re1 == re2
  }

  test("test 1") {
    forAll(Gen.sized(GenRegex.genRegex)) { re =>
      println(re)
      check2(re)
    }
  }

  test("test 2") {
    //eval
    assert(ArithEval.eval(Num(1)) == (1))
    assert(ArithEval.eval(Add(Num(1), Num(2))) == (3))
    assert(ArithEval.eval(Sub(Num(10), Num(2))) == (8))
    assert(ArithEval.eval(Mul(Num(5), Num(5))) == (25))
    assert(ArithEval.eval(Div(Num(20), Num(4))) == (5))
    assert(ArithEval.eval(Exponent(Num(3), Num(2))) == (9))
    }

  test("test 3") {
    //parser
    def check(e: Expr) = {
      val atom = ArithParser.parseAll(ArithParser.atom, ArithPrinter.print(e)).get
      val exponent = ArithParser.parseAll(ArithParser.exponent, ArithPrinter.print(e)).get
      val add = ArithParser.parseAll(ArithParser.add, ArithPrinter.print(e)).get
      val mul = ArithParser.parseAll(ArithParser.mul, ArithPrinter.print(e)).get
      val expr = ArithParser.parseAll(ArithParser.expr, ArithPrinter.print(e)).get
      assert(e == atom)
      assert(e == exponent)
      assert(e == add)
      assert(e == mul)
      assert(e == expr)
      }
    }

  test("test 4") {
    //print
    def check(str: String) = {
      val atom = ArithParser.parseAll(ArithParser.atom, str).get
      val atom2 =  ArithParser.parseAll(ArithParser.atom, ArithPrinter.print(atom)).get
      val exponent = ArithParser.parseAll(ArithParser.expr, str).get
      val exponent2 = ArithParser.parseAll(ArithParser.expr, ArithPrinter.print(exponent)).get 
      val add = ArithParser.parseAll(ArithParser.expr, str).get
      val add2 = ArithParser.parseAll(ArithParser.expr, ArithPrinter.print(add)).get 
      val mul =  ArithParser.parseAll(ArithParser.expr, str).get 
      val mul2 = ArithParser.parseAll(ArithParser.expr, ArithPrinter.print(mul)).get
      val expr = ArithParser.parseAll(ArithParser.expr, str).get 
      val expr2 = ArithParser.parseAll(ArithParser.expr, ArithPrinter.print(expr)).get
      assert(atom == atom2)
      assert(exponent == exponent2)
      assert(add == add2)
      assert(mul == mul2)
      assert(expr == expr2)
      }
    }

}