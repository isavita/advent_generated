object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines.toList

  val passports = input.mkString("\n").split("\n\n")

  def validate(passport: String): Boolean = {
    val fields = passport.split("[ \n]").map(_.split(":"))
    val fieldNames = fields.map(_(0))

    val requiredFields = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    if (requiredFields.forall(fieldNames.contains)) {
      fields.forall {
        case Array("byr", value) => value.matches("19[2-9][0-9]|200[0-2]")
        case Array("iyr", value) => value.matches("201[0-9]|2020")
        case Array("eyr", value) => value.matches("202[0-9]|2030")
        case Array("hgt", value) => value.matches("(1[5-8][0-9]|19[0-3])cm|(59|6[0-9]|7[0-6])in")
        case Array("hcl", value) => value.matches("#[0-9a-f]{6}")
        case Array("ecl", value) => value.matches("amb|blu|brn|gry|grn|hzl|oth")
        case Array("pid", value) => value.matches("[0-9]{9}")
        case _ => true
      }
    } else {
      false
    }
  }

  val validPassports = passports.count(validate)

  println(validPassports)
}