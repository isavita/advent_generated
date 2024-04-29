def countUniqueDigits(String filename) {
    def count = 0
    new File(filename).eachLine { line ->
        def parts = line.split(' \\| ')
        def signals = parts[0].split(' ').collect { it }.unique().size()
        def output = parts[1].split(' ').collect { it.size() }
        output.each { size ->
            if ([2, 4, 3, 7].contains(size)) {
                count++
            }
        }
    }
    count
}

println "The unique digits appear ${countUniqueDigits('input.txt')} times."