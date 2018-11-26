import scala.util.matching.Regex

object Regexes extends hw.regex.RegexLike {
  def notAlphanumeric: Regex = "[^A-Za-z0-9]*".r
  def time: Regex = """((2[0-3])|([0-1][0-9])):[0-5][0-9]""".r
  def phone: Regex = """\(\d{3}\) \d{3}-\d{4}""".r
  def zip: Regex = """(\d{5}-\d{4})|\d{5}""".r
  def comment: Regex = """/\*.*\*/""".r
  def numberPhrase: Regex = """(twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety)(-(one|two|three|four|five|six|seven|eight|nine))?""".r
  def roman: Regex = """([X]{0,3})((I)|(II)|(III)|(IV)|(V)|(VI)|(VII)|(VIII)|(IX))""".r
  def date: Regex = """(((\d{4})-(0[13578]|1[02])-(0[1-9]|[12]\d|3[01]))|((\d{4})-(0[469]|11)-(0[1-9]|[12]\d|30))|((\d{4})-02-(0[1-9]|1\d|2[0-8]))|(([0-9][0-9](00|04|08|12|16|20|24|28|32|36|40|44|48|52|56|60|64|68|72|76|80|84|88|92|96))-02-29))""".r
  def evenParity: Regex = """(([02468]*[13579]){2})*[02468]*""".r
}