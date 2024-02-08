def input = new File("input.txt").text.trim()

def programs = ('a'..'p').toList()

input.split(',').each {
    switch (it[0]) {
        case 's':
            programs = programs.drop(programs.size() - it[1..-1].toInteger()) + programs.take(programs.size() - it[1..-1].toInteger())
            break
        case 'x':
            def (a, b) = it[1..-1].split('/').collect { it as int }
            def temp = programs[a]
            programs[a] = programs[b]
            programs[b] = temp
            break
        case 'p':
            def (a, b) = it[1..-1].split('/')
            def indexA = programs.indexOf(a)
            def indexB = programs.indexOf(b)
            def temp = programs[indexA]
            programs[indexA] = programs[indexB]
            programs[indexB] = temp
            break
    }
}

println(programs.join(''))