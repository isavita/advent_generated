
def root = ['']
def dirs = [:]
def files = [:]
def curr = []
def input = new File('input.txt').text.readLines()

input.each { line ->
    def txt = line.split()
    if (txt[0] == '$') {
        if (txt[1] == 'cd') {
            if (txt[2] == '/') {
                curr = root
            } else if (txt[2] == '..') {
                curr = curr[0..-2]
            } else {
                curr << txt[2]
            }
            dirs[curr.join('/')] = 0
        }
    } else if (txt[0] != 'dir') {
        files[curr.join('/') + '/' + txt[1]] = txt[0].toInteger()
    }
}

files.each { f, s ->
    def path = f.split('/')
    (1..<path.length).each { i ->
        dirs[path[0..i-1].join('/')] = dirs.get(path[0..i-1].join('/'), 0) + s
    }
}

def sortedSizes = dirs.values().sort()
def total = 70000000
def want = 30000000
def available = total - dirs['']
println sortedSizes.find { it >= want - available }
