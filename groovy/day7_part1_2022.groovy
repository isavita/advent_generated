
import groovy.json.JsonSlurper

class Directory {
    String name
    long size = 0
    List<Directory> subdirectories = []

    Directory(String name) {
        this.name = name
    }

    void addSize(long fileSize) {
        this.size += fileSize
    }

    long getTotalSize() {
        long total = size
        for (dir in subdirectories) {
            total += dir.getTotalSize()
        }
        return total
    }
}

def parseInput(File file) {
    def root = new Directory("/")
    def currentDir = root
    def dirStack = []

    file.eachLine { line ->
        if (line.startsWith('$')) {
            def command = line.substring(2).trim()
            if (command.startsWith('cd ')) {
                def dirName = command.substring(3).trim()
                if (dirName == '..') {
                    currentDir = dirStack.pop()
                } else if (dirName == '/') {
                    currentDir = root
                } else {
                    def newDir = new Directory(dirName)
                    currentDir.subdirectories.add(newDir)
                    dirStack.push(currentDir)
                    currentDir = newDir
                }
            }
            // 'ls' command does not change the current directory
        } else {
            if (line.startsWith('dir ')) {
                // Directory line, do nothing as we handle it in 'cd'
            } else {
                def parts = line.split(' ')
                def fileSize = Long.parseLong(parts[0])
                currentDir.addSize(fileSize)
            }
        }
    }
    return root
}

def calculateSumOfSmallDirectories(Directory dir, long limit) {
    long sum = 0
    long totalSize = dir.getTotalSize()
    if (totalSize <= limit) {
        sum += totalSize
    }
    for (subdir in dir.subdirectories) {
        sum += calculateSumOfSmallDirectories(subdir, limit)
    }
    return sum
}

def main() {
    File inputFile = new File("input.txt")
    Directory root = parseInput(inputFile)
    long result = calculateSumOfSmallDirectories(root, 100000)
    println result
}

main()
