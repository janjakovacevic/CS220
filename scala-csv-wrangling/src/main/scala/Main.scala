import  hw.csv._

sealed trait Gender
case class Male() extends Gender
case class Female() extends Gender

case class SSARow(birthYear: Int, name: String, gender: Gender, count: Int)

case class CDCRow(birthYear: Int, maleLifeExpectancy: Int,
  femaleLifeExpectancy: Int)

object Main {

  def readSSARow(row: List[String]): SSARow = {
    SSARow(row(0).toInt, row(1),
      if(row(2) == "M") Male() else Female(),
      row(3).toInt)
   }

  def readCDCRow(row: List[String]): CDCRow = {
     CDCRow(row(0).toInt, row(1).toInt, row(2).toInt)
    }

  def yearIs(rows: List[SSARow], bound: Int): List[SSARow] = {
    rows.filter(row => row.birthYear == bound)
    }

  def yearGT(rows: List[SSARow], bound: Int): List[SSARow] = {
    rows.filter(row => row.birthYear >= bound)
    }

  def yearLT(rows: List[SSARow], bound: Int): List[SSARow] = {
    rows.filter(row => row.birthYear <= bound)
    }

  def onlyName(rows: List[SSARow], name: String): List[SSARow] = {
    rows.filter(row => row.name == name)
    }

  def mostPopular(rows: List[SSARow]): (String, Int) = {
    if(rows == List()) throw new Exception("invalid argument")
    else rows.groupBy(row => row.name).map(umass => (umass._1, count(umass._2))).maxBy(_._2)
    }

  def count(rows: List[SSARow]): Int = rows match{
    case Nil => 0
    case head :: tail => head.count + count(tail)
    }

  def countGirlsAndBoys(rows: List[SSARow]): (Int, Int) = {
    (count(rows.filter(row => row.gender == Female())), 
     count(rows.filter(row => row.gender == Male())))
    }

  def genderNeutralNames(rows: List[SSARow]): Set[String] = {
     ((rows.filter(row => row.gender == Female()).map(row => row.name)).toSet).intersect((rows.filter(row => row.gender == Male()).map(row => row.name)).toSet)
    }

  def expectedAlive(gender: Gender, birthYear: Int, currentYear: Int,
    lifeExpectancies: List[CDCRow]): Boolean = (gender == Female()) match {
          case true => (lifeExpectancies.filter(lifeExpectancies => lifeExpectancies.birthYear == 
            birthYear)).map(lifeExpectancies => lifeExpectancies.femaleLifeExpectancy) match {
              case Nil => false //throw new Exception("invalid argument")
              case head :: tail => birthYear + head >= currentYear
            }
          case false => (lifeExpectancies.filter(lifeExpectancies => lifeExpectancies.birthYear == 
            birthYear)).map(lifeExpectancies => lifeExpectancies.maleLifeExpectancy) match {
              case Nil => false //throw new Exception("invalid argument")
              case head :: tail => birthYear + head >= currentYear
            }
        }

  def estimatePopulation(rows: List[SSARow], year: Int, 
    lifeExpectancies: List[CDCRow]): Int = rows match {
      case Nil => 0
      case head :: tail => expectedAlive(head.gender, head.birthYear, year, lifeExpectancies) match {
        case true => head.count + estimatePopulation(tail, year, lifeExpectancies)
        case false => estimatePopulation(tail, year, lifeExpectancies)
        }
      }

}