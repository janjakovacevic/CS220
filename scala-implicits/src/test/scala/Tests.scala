class Tests extends org.scalatest.FunSuite {

  import PathImplicits._
  import TimeImplicits._

  import java.nio.file._
  import java.io._
  import java.time.LocalDate

  import java.time.Month

  test("It would be convenient if we could build a path by using / to separate strings") {
    assert("usr" / "bin" / "scala" == Paths.get("usr/bin/scala"))
  }

  test("The / operator should be able to put two paths together too") {
    val p1 = "usr" / "local"
    val p2 = "bin" / "scala"
    assert(p1/p2 == Paths.get("usr/local/bin/scala"))
  }

  test("Paths should have a .write method to create files") {
    val p = Paths.get("test.txt")
    val contents = "This should write a file"
    try {
      p.write(contents)
      assert(new String(Files.readAllBytes(p)) == contents)
    }
    finally {
      Files.deleteIfExists(p)
    }
  }

  test("Paths should have a .read method to read files") {
    val p = Paths.get("test.txt")
    val contents = "This should write a file"
    try {
      Files.write(p, contents.getBytes)
      assert(p.read() == contents)
    }
    finally {
      Files.deleteIfExists(p)
    }
  }

  test("Paths should have a .append method to append data to the end of a file") {
    val p = Paths.get("test.txt")
    val contents = "First line\nSecond"
    try {
      p.append("First line\n")
      p.append("Second line\n")
     assert(new String(Files.readAllBytes(p)) == "First line\nSecond line\n")
    }
    finally {
      Files.deleteIfExists(p)
    }
  }

  test("This is a convenient way of writing a date in the current year") {
    assert(15.jan == LocalDate.of(2018, 1, 15))
    assert(28.feb == LocalDate.of(2018, 2, 28))
    assert(2.mar == LocalDate.of(2018, 3, 2))
    intercept[Exception] {
      val bad = 29.feb
      val bad2 = 30.feb
    }
  }

  test("We can write dates in other years like this") {
    val date1 = 28 feb 2015
    assert(date1 == LocalDate.of(2015, 2, 28))
    val date2 = 15 oct 1989
    assert(date2 == LocalDate.of(1989, 10, 15))
  }

  test("We can add days to a date") {
    val date1 = LocalDate.of(2016, 1, 31) + 2.days
    assert(date1 == LocalDate.of(2016, 2, 2))
  }

  test("We can add days to a date written using our date DSL") {
    val date1 = (31 jan 2016) + 2.days
    assert(date1 == LocalDate.of(2016, 2, 2))
  }

  test("We can add months to a date") {
    val date1 = LocalDate.of(2016, 1, 31) + 3.months
    assert(date1 == LocalDate.of(2016, 4, 30))
  }

  test("We can add years to a date") {
    val date1 = LocalDate.of(2016, 1, 31) + 5.years
    assert(date1 == LocalDate.of(2021, 1, 31))
  }

// NEW TESTS

  val path = Paths.get("a", "b")
	val path2 = Paths.get("c", "d")
	//create a test folder inside implicits folder
	val path3 = Paths.get("test","test.txt")

	test("test / 1") {
		assert("a"/("b") == "a"/"b")
	}

	test("test / 2") {
		assert(path/("c") == "a"/"b"/"c")
	}

	test("test / 3") {
		assert(path/(path2) == "a"/"b"/"c"/"d")
	}

	test("test write") {
		assert(path3.write("hello \n") == path3)
	} 

	test("test read"){
		assert(path3.read == "hello \n")
	}

	test("test append"){
		assert(path3.append("it's me").read == "hello \nit's me")
	}

	test("test date1"){
		assert(2.jan == LocalDate.of(2018, 1, 2))
	}

	test("test date2"){
		assert(2.jan(2012) == LocalDate.of(2012, 1, 2))
	}

	test("test date3"){
		assert(29.feb(2016) == LocalDate.of(2016, 2, 29))
	}

	test("test date4"){
		assert(29.feb(2016) +(2.days) == LocalDate.of(2016, 3, 2))
	}

// MORE TESTS

  val constant_path0 = Paths.get("usr", "bin")
	val constant_path1 = Paths.get("usr", "bin", "scala")
	val constant_path2 = Paths.get("usr", "local", "bin", "scala")

	test("string to string path works"){
		assert(("usr" / "bin") == constant_path0) //concat string to string
		assert((("usr" / "bin") / "scala") == constant_path1) //concat string to path
		assert(("usr" / ("bin" / "scala")) == constant_path1) //concat path to string

		val a = "usr" / "local"
		val b = "bin" / "scala"

		assert((a / b) == constant_path2) //concat path to path
	}

	test("file read write append works"){
		val testfile = "." / "tennis"
		Files.deleteIfExists(testfile)
		testfile.write("tennis ")
		assert(testfile.read() == "tennis ")
		testfile.append("again")
		assert(testfile.read == "tennis again")

	}

	test("date generation [day] [month] works"){
		assert(10.jan == LocalDate.of(LocalDate.now().getYear(), Month.JANUARY, 10))
		assert(10.feb == LocalDate.of(LocalDate.now().getYear(), Month.FEBRUARY, 10))
		assert(10.mar == LocalDate.of(LocalDate.now().getYear(), Month.MARCH, 10))
		assert(10.apr == LocalDate.of(LocalDate.now().getYear(), Month.APRIL, 10))
		assert(10.may == LocalDate.of(LocalDate.now().getYear(), Month.MAY, 10))
		assert(10.jun == LocalDate.of(LocalDate.now().getYear(), Month.JUNE, 10))
		assert(10.jul == LocalDate.of(LocalDate.now().getYear(), Month.JULY, 10))
		assert(10.aug == LocalDate.of(LocalDate.now().getYear(), Month.AUGUST, 10))
		assert(10.sep == LocalDate.of(LocalDate.now().getYear(), Month.SEPTEMBER, 10))
		assert(10.oct == LocalDate.of(LocalDate.now().getYear(), Month.OCTOBER, 10))
    assert(10.nov == LocalDate.of(LocalDate.now().getYear(), Month.NOVEMBER, 10))
		assert(10.dec == LocalDate.of(LocalDate.now().getYear(), Month.DECEMBER, 10))
	}

	test("compound date generation works"){
		assert((31 jan 2016) + 2.days ==  LocalDate.of(2016, 2, 2))
		assert((31 jan 2016) + 2.months ==  LocalDate.of(2016, 3, 31))
		assert((31 jan 2016) + 2.years ==  LocalDate.of(2018, 1, 31))
    assert((31 jan 2018) + 1.months == LocalDate.of(2018, 2, 28))
    assert((31 jan 2016) + 1.months == LocalDate.of(2016, 2, 29))
    assert((31 dec 2015) + 2.months == LocalDate.of(2016, 2, 29))
	}
}
