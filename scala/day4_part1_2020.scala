import scala.io.Source

object PassportProcessing extends App {
  val input = Source.fromFile("input.txt").getLines().toList
  val passports = input.mkString(" ").split("  ").map(_.split("\\s+").map(_.split(":")(0)).toSet)
  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val validPassports = passports.count(p => requiredFields.diff(p).size == 0 || requiredFields.diff(p) == Set("cid"))
  println(validPassports)
}