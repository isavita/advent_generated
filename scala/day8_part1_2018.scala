
object Main extends App {
  val input = scala.io.Source.fromFile("input.txt").getLines().next().split(" ").map(_.toInt).toList

  case class Node(children: List[Node], metadata: List[Int])

  def parseNode(nums: List[Int]): (Node, List[Int]) = {
    val numChildren = nums.head
    val numMetadata = nums(1)
    var remaining = nums.drop(2)
    var children = List.empty[Node]

    for (_ <- 0 until numChildren) {
      val (child, rem) = parseNode(remaining)
      children = children :+ child
      remaining = rem
    }

    val metadata = remaining.take(numMetadata)
    val node = Node(children, metadata)
    (node, remaining.drop(numMetadata))
  }

  def sumMetadata(node: Node): Int = {
    val childSum = node.children.map(sumMetadata).sum
    val metadataSum = node.metadata.sum
    childSum + metadataSum
  }

  val (root, remaining) = parseNode(input)
  val result = sumMetadata(root)

  println(result)
}
