class TrivialTestSuite extends org.scalatest.FunSuite {

  import Regexes._

  test("The Regexes object must be defined") {
    val regexes: hw.regex.RegexLike = Regexes
  }

//11=

  val x1 = "*!(#!"
	val x2 = "An|-|CuongDepTraiVC()@''[p"
	val b1 = "00:00"
	val b2 = "23:42"
	val b3 = "23:00"
	val c1 = "(206) 313-1808"
	val c2 = "(000) 125-6894"	
	val d1 = "02149"
	val d2 = "02149-1314"
	val e = "/*sadsadad21321@DSA*/"
	val e2 = "/*hello\nworld*/"
	val f = "forty-six"
	val f2 = "ninety"
	val g = "XXXVIII"
	val g2 = "XIV"
	val g3 = "VIII"
	val h1 = "2016-02-29"
	val h2 = "2017-03-31"
	val h3 = "2000-02-28"
  val h4 = "2015-02-29"

	test("a1"){
		assert(notAlphanumeric.pattern.matcher(x1).matches)
	}
	test("a2"){
		assert(notAlphanumeric.pattern.matcher(x2).matches == false)
	}
	test("b1"){
		assert(time.pattern.matcher(b1).matches)
	}
	test("b2"){
		assert(time.pattern.matcher(b2).matches)
	}
	test("b3"){
		assert(time.pattern.matcher(b3).matches)
	}
	test("c1"){
		assert(phone.pattern.matcher(c1).matches)
	}
	test("c2"){
		assert(phone.pattern.matcher(c2).matches)
	}
	test("d1"){
		assert(zip.pattern.matcher(d1).matches)
	}
	test("d2"){
		assert(zip.pattern.matcher(d2).matches)
	}
	test("e"){
		assert(comment.pattern.matcher(e).matches)
	}
/*
	test("e2") {
		assert(comment.pattern.matcher(e2).matches)
	}
*/
	test("f"){
		assert(numberPhrase.pattern.matcher(f).matches)
	}

	test("f2") {
		assert(numberPhrase.pattern.matcher(f2).matches)
	}

	test("g"){
		assert(roman.pattern.matcher(g).matches)
	}

	test ("g2") {
		assert(roman.pattern.matcher(g2).matches)
	}

	test ("g3") {
		assert(roman.pattern.matcher(g3).matches)
	}
	test("h1"){
		assert(date.pattern.matcher(h1).matches)
	}
	test("h2"){
		assert(date.pattern.matcher(h2).matches)
	}
	test("h3"){
		assert(date.pattern.matcher(h3).matches)
	}
  test("h4"){
    assert(date.pattern.matcher(h4).matches == false)
    }
	test("z"){
		assert(evenParity.pattern.matcher("00010357").matches)
	}

//2

  test("time match works"){
		val t = time.pattern
		assert(t.matcher("00:00").matches)
		assert(t.matcher("23:59").matches)
	}

	test("comment match works"){
		val c = comment.pattern
		assert(c.matcher("/**/").matches)
		assert(c.matcher("/*l*/").matches)
		assert(c.matcher("/*lllllllgggfdasfahguwaeghauefj*/").matches)
	}

//3

	test("notAlphanumeric test"){
		val str = "abc"
		assert(notAlphanumeric.pattern.matcher(str).matches() == false)
	}
	test("notAlphanumeric test 2"){
		val str = "!@(!)*$!@($)*)!@$"
		assert(notAlphanumeric.pattern.matcher(str).matches() == true)
	}

	test("time test"){
		val str = "12:30"
		assert(time.pattern.matcher(str).matches() == true)
	}
	test("time test 2"){
		val str = "25:30"
		assert(time.pattern.matcher(str).matches() == false)
	}
	test("phone number test"){
		val num = "(123) 456-7890"
		assert(phone.pattern.matcher(num).matches() == true)

	}
	test("phone number test 2"){
		val num = "(123)456-7890"
		assert(phone.pattern.matcher(num).matches() == false)
	}
	test("zip test"){
		val str = "12345"
		assert(zip.pattern.matcher(str).matches == true)
	}
	test("zip test 2"){
		val str = "12345-6789"
		assert(zip.pattern.matcher(str).matches == true)
	}
	test("zip test 3"){
		val str = "123456"
		assert(zip.pattern.matcher(str).matches == false)
	}
	test("comment test"){
		val str = "/*hello */"
		assert(comment.pattern.matcher(str).matches == true)
	}
	test("comment test 2"){
		val str = "hello"
		assert(comment.pattern.matcher(str).matches == false)
	}
	test("numberPhrase test"){
		val str = "thirty-one"
		val str2= "ninety"
		assert(numberPhrase.pattern.matcher(str).matches == true)
		assert(numberPhrase.pattern.matcher(str2).matches == true)
	}
	test("roman test"){
		val str = "VI"
		assert(roman.pattern.matcher(str).matches == true)
	}
	test("date test"){
		val pass = "1990-08-30"
		val pass2 = "1990-09-30"
		val pass3 = "1996-02-29"
		val fail = "1990-02-29"
		val pass4 = "2017-02-28"
		assert(date.pattern.matcher(pass).matches == true)
		assert(date.pattern.matcher(pass2).matches == true)
		assert(date.pattern.matcher(pass3).matches == true)
		assert(date.pattern.matcher(pass4).matches == true)
		assert(date.pattern.matcher(fail).matches == false)
	}
	test("evenParity test"){
		val pass = "1032507"
		val fail = "20114423"
		val pass2 = "0002"
		assert(evenParity.pattern.matcher(pass).matches == true)
		assert(evenParity.pattern.matcher(pass2).matches == true)
		assert(evenParity.pattern.matcher(fail).matches == false)
	}
  
}
