object PathImplicits {
  import java.nio.file._
  import java.io._

  implicit class RichString(string: String){
    def /(s: String): Path = Paths.get(string, s)
    def /(path: Path): Path = Paths.get(string).resolve(path)
    }

  implicit class RichPath(path: Path){
    def /(string: String): Path = path.resolve(string)
    def /(p: Path): Path = path.resolve(p)

    def write(string: String): Path = Files.write(path, string.getBytes)
    def read(): String = new String(Files.readAllBytes(path))
    def append(string: String): Path = {
      if(Files.exists(path) == false) path.write(string)
      else Files.write(path, string.getBytes, StandardOpenOption.APPEND)
      }
    }
}

object TimeImplicits {
  import java.time.LocalDate

  implicit class RichLocalDate(number: Int){
    def days(): (Int, Int) = (number, 1)
    def months(): (Int, Int) = (number, 2)
    def years(): (Int, Int) = (number, 3)

    def jan(): LocalDate = LocalDate.of(LocalDate.now.getYear, 1, number)
    def feb(): LocalDate = LocalDate.of(LocalDate.now.getYear, 2, number)
    def mar(): LocalDate = LocalDate.of(LocalDate.now.getYear, 3, number)
    def apr(): LocalDate = LocalDate.of(LocalDate.now.getYear, 4, number)
    def may(): LocalDate = LocalDate.of(LocalDate.now.getYear, 5, number)
    def jun(): LocalDate = LocalDate.of(LocalDate.now.getYear, 6, number)
    def jul(): LocalDate = LocalDate.of(LocalDate.now.getYear, 7, number)
    def aug(): LocalDate = LocalDate.of(LocalDate.now.getYear, 8, number)
    def sep(): LocalDate = LocalDate.of(LocalDate.now.getYear, 9, number)
    def oct(): LocalDate = LocalDate.of(LocalDate.now.getYear, 10, number)
    def nov(): LocalDate = LocalDate.of(LocalDate.now.getYear, 11, number)
    def dec(): LocalDate = LocalDate.of(LocalDate.now.getYear, 12, number)
  
    def jan(year: Int): LocalDate = LocalDate.of(year, 1, number)
    def feb(year: Int): LocalDate = LocalDate.of(year, 2, number)
    def mar(year: Int): LocalDate = LocalDate.of(year, 3, number)
    def apr(year: Int): LocalDate = LocalDate.of(year, 4, number)
    def may(year: Int): LocalDate = LocalDate.of(year, 5, number)
    def jun(year: Int): LocalDate = LocalDate.of(year, 6, number)
    def jul(year: Int): LocalDate = LocalDate.of(year, 7, number)
    def aug(year: Int): LocalDate = LocalDate.of(year, 8, number)
    def sep(year: Int): LocalDate = LocalDate.of(year, 9, number)
    def oct(year: Int): LocalDate = LocalDate.of(year, 10, number)
    def nov(year: Int): LocalDate = LocalDate.of(year, 11, number)
    def dec(year: Int): LocalDate = LocalDate.of(year, 12, number) 
  }

  implicit class RichDate(date: LocalDate){
    def +(tuple: (Int, Int)) = {
      if(tuple._2 == 1) date.plusDays(tuple._1)
      else if(tuple._2 == 2) date.plusMonths(tuple._1)
      else if(tuple._2 == 3) date.plusYears(tuple._1)
      else throw new Exception("illegal argument")
      }
    }

}