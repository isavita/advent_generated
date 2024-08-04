import java.io.File

class Directory(
    val name: String,
    val parent: Directory? = null,
    val files: MutableMap<String, Int> = mutableMapOf(),
    val directories: MutableMap<String, Directory> = mutableMapOf()
) {
    fun size(): Int {
        return files.values.sum() + directories.values.sumOf { it.size() }
    }
}

fun main() {
    val root = Directory("/")
    var currentDirectory = root

    File("input.txt").readLines().forEach { line ->
        if (line.startsWith("$")) {
            val command = line.substring(2)
            if (command.startsWith("cd")) {
                val directoryName = command.substring(3)
                when (directoryName) {
                    "/" -> currentDirectory = root
                    ".." -> currentDirectory = currentDirectory.parent!!
                    else -> currentDirectory = currentDirectory.directories[directoryName]!!
                }
            }
        } else {
            if (line.startsWith("dir")) {
                val directoryName = line.substring(4)
                currentDirectory.directories[directoryName] = Directory(directoryName, currentDirectory)
            } else {
                val (size, fileName) = line.split(" ")
                currentDirectory.files[fileName] = size.toInt()
            }
        }
    }

    val sizes = mutableListOf<Int>()
    fun calculateSizes(directory: Directory) {
        sizes.add(directory.size())
        directory.directories.values.forEach { calculateSizes(it) }
    }
    calculateSizes(root)

    val sum = sizes.filter { it <= 100000 }.sum()
    println(sum)
}