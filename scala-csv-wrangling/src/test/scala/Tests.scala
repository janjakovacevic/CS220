import hw.csv._
import Main._

class TestSuite extends org.scalatest.FunSuite {

 // val allBirths = CSVReader.fromFile("ssa-births.csv")

  val ssaRowList: List[String] = List("1880", "Mary", "F", "7065")
  val ssaRowList2: List[String] = List("1880", "Jim", "M", "345")
  val cdcRowList: List[String] = List("2010", "76", "81")
  val list1: List[SSARow] = List(SSARow(1880, "Johanna", Female(), 64), 
                                 SSARow(1880, "Anna", Male(), 12),
                                 SSARow(1882, "Lucy", Female(), 673))
  val list2: List[SSARow] = List(SSARow(1885, "John", Male(), 8756), 
                                 SSARow(1890, "Lawrence", Female(), 6), 
                                 SSARow(1893, "Isabella", Female(), 109))
  val list3: List[SSARow] = List(SSARow(1897, "Clair", Male(), 27), 
                                 SSARow(1900, "Olga", Female(), 376),
                                 SSARow(1904, "Lenord", Male(), 5))
  val list4: List[SSARow] = List(SSARow(1899, "Frank", Female(), 21), 
                                 SSARow(1907, "Ernestine", Female(), 143), 
                                 SSARow(1910, "Geraldine", Female(), 551))
  val list5: List[SSARow] = List(SSARow(1898, "Stanford", Male(), 7), 
                                 SSARow(1901, "Emmitt", Male(), 16), 
                                 SSARow(1904, "Less", Male(), 6))
  val list6: List[SSARow] = List(SSARow(1911, "Henry", Male(), 3167), 
                                 SSARow(1912, "Helene", Female(), 378), 
                                 SSARow(1918, "Henry", Male(), 11354),
                                 SSARow(1923, "Henry", Female(), 81),
                                 SSARow(1891, "Helene", Female(), 68),
                                 SSARow(1896, "Helene", Female(), 100))
  val list7: List[SSARow] = List(SSARow(1894, "Clara", Female(), 2603),
                                 SSARow(1895, "Valentine", Male(), 23),
                                 SSARow(1897, "Nervie", Female(), 5),
                                 SSARow(1900, "Elizabeth", Female(), 4096),
                                 SSARow(1901, "Palmer", Male(), 20))
  val list8: List[SSARow] = List(SSARow(1913, "Lucille", Female(), 4098),
                                 SSARow(1915, "Fortuna", Female(), 6),
                                 SSARow(1918, "Angeline", Female(), 949),
                                 SSARow(1921, "Kathyrn", Female(), 28),
                                 SSARow(1924, "Jene", Male(), 12),
                                 SSARow(1925, "Timothy", Male(), 319))
  val list9: List[SSARow] = List(SSARow(1927, "Seroba", Female(), 8),
                                 SSARow(1928, "Rebecca", Female(), 1122),
                                 SSARow(1936, "Rebecca", Female(), 1201),
                                 SSARow(1940, "Rebecca", Female(), 2555),
                                 SSARow(1943, "Rebecca", Female(), 3268))
  val list10: List[SSARow] = List(SSARow(1944, "Frank", Female(), 40),
                                  SSARow(1946, "Frank", Female(), 51),
                                  SSARow(1887, "Frank", Male(), 2883),
                                  SSARow(1886, "Jesse", Male(), 6),
                                  SSARow(1888, "Jesse", Female(),12),
                                  SSARow(1885, "Bernard", Male(), 160),
                                  SSARow(1886, "Lelar", Female(), 11))
  val list11: List[CDCRow] = List(CDCRow(2010, 76, 81),
                                  CDCRow(2009, 75, 80),
                                  CDCRow(2008, 75, 80),
                                  CDCRow(2007, 75, 80),
                                  CDCRow(2006, 75, 80),
                                  CDCRow(2005, 75, 80),
                                  CDCRow(2004, 75, 80))
  val list12: List[CDCRow] = List(CDCRow(1880, 45, 50),
                                  CDCRow(1886, 50, 55),
                                  CDCRow(1894, 55, 60),
                                  CDCRow(1944, 60, 65))

  test("readSSARow test"){
    assert(readSSARow(ssaRowList) == SSARow(1880, "Mary", Female(), 7065))
    assert(readSSARow(ssaRowList2) == SSARow(1880, "Jim", Male(), 345))
    }

  test("readCDCRow test"){
    assert(readCDCRow(cdcRowList) == CDCRow(2010, 76, 81))
    }

  test("yearIs test"){
    assert(yearIs(list1, 1880) == List(SSARow(1880, "Johanna", Female(), 64), 
                                       SSARow(1880, "Anna", Male(), 12)))
    assert(yearIs(list6, 1920) == List())
    }

  test("yearGT test"){
    assert(yearGT(list2, 1886) == List(SSARow(1890, "Lawrence", Female(), 6), 
                                       SSARow(1893, "Isabella", Female(), 109)))
    assert(yearGT(list9, 2000) == List())
    }  

  test("yearLT test"){
    assert(yearLT(list3, 1900) == List(SSARow(1897, "Clair", Male(), 27), 
                                       SSARow(1900, "Olga", Female(), 376)))
    assert(yearLT(list7, 1893) == List())
    } 

  test("onlyName test"){
    assert(onlyName(list7, "Valentine") == List(SSARow(1895, "Valentine", Male(), 23)))
    assert(onlyName(list6, "Janja") == List())
    assert(onlyName(list9, "Rebecca") == List(SSARow(1928, "Rebecca", Female(), 1122),
                                              SSARow(1936, "Rebecca", Female(), 1201),
                                              SSARow(1940, "Rebecca", Female(), 2555),
                                              SSARow(1943, "Rebecca", Female(), 3268)))
    }   

  test("mostPopular test"){
    assert(mostPopular(list6) == ("Henry", 14602))
    assert(mostPopular(list10) == ("Frank", 2974))
    assertThrows[Exception]{mostPopular(List())}
    }  
  
  test("count test"){
    assert(count(list4) == 715)
    assert(count(list6) == 15148)
    assert(count(List()) == 0)
    }  

  test("countGirlsAndBoys test"){
    assert(countGirlsAndBoys(list3) == (376, 32))
    assert(countGirlsAndBoys(list4) == (715, 0)) 
    assert(countGirlsAndBoys(list5) == (0, 29))
    assert(countGirlsAndBoys(list6) == (627, 14521))
    assert(countGirlsAndBoys(List()) == (0, 0))
    }  

  test("genderNeutralNames test"){
    assert(genderNeutralNames(list6) == Set("Henry"))
    assert(genderNeutralNames(list10) == Set("Frank", "Jesse"))
    assert(genderNeutralNames(list9) == Set())
    } 

  test("expectedAlive test"){
    assert(expectedAlive(Female(), 2009, 2018, list11) == true)
    assert(expectedAlive(Male(), 2007, 2082, list11) == true)
    assert(expectedAlive(Male(), 2005, 2100, list11) == false) 
    }

  test("estimatePopulation test"){
    assert(estimatePopulation(list1, 1900, list12) == 76)
    assert(estimatePopulation(list10, 1940 ,list12) == 51)
    assert(estimatePopulation(List(), 2000, list12) == 0)
    }  
}