def input = new File('input.txt').text

def json = new groovy.json.JsonSlurper().parseText(input)

def sumNumbers(json) {
    if (json instanceof Integer) {
        return json
    } else if (json instanceof List) {
        return json.collect { sumNumbers(it) }.sum()
    } else if (json instanceof Map) {
        if (json.containsValue("red")) {
            return 0
        } else {
            return json.values().collect { sumNumbers(it) }.sum()
        }
    } else {
        return 0
    }
}

println sumNumbers(json)