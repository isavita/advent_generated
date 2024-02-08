
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().toList

  def isNice(s: String): Boolean = {
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val disallowed = Set("ab", "cd", "pq", "xy")
    
    def hasThreeVowels(s: String): Boolean = s.count(vowels.contains) >= 3
    def hasDoubleLetter(s: String): Boolean = s.sliding(2).exists(pair => pair(0) == pair(1))
    def doesNotContainDisallowed(s: String): Boolean = !disallowed.exists(s.contains)
    
    hasThreeVowels(s) && hasDoubleLetter(s) && doesNotContainDisallowed(s)
  }

  val niceStrings = input.count(isNice)
  println(niceStrings)
}
