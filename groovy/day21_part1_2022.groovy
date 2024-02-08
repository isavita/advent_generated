
def file = new File("input.txt")
def jobs = [:]
def results = [:]

file.eachLine { line ->
    def parts = line.split(": ")
    jobs[parts[0]] = parts[1]
}

println calculate("root", jobs, results)

def calculate(String monkey, Map jobs, Map results) {
    if (results.containsKey(monkey)) {
        return results[monkey]
    }

    if (!jobs.containsKey(monkey)) {
        throw new Exception("Monkey not found: $monkey")
    }

    def job = jobs[monkey]

    if (job.isNumber()) {
        def num = job as Integer
        results[monkey] = num
        return num
    }

    def parts = job.tokenize()
    def a = calculate(parts[0], jobs, results)
    def b = calculate(parts[2], jobs, results)

    def result
    switch (parts[1]) {
        case "+":
            result = a + b
            break
        case "-":
            result = a - b
            break
        case "*":
            result = a * b
            break
        case "/":
            result = a / b
            break
        default:
            throw new Exception("Unknown operation: ${parts[1]}")
    }

    results[monkey] = result
    return result
}
