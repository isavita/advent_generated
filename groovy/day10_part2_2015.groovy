def input = new File("input.txt").text.trim()

def lookAndSay = { str ->
    str.replaceAll(/(.)\1*/) { match ->
        "${match[0].length()}${match[0][0]}"
    }
}

(1..50).each {
    input = lookAndSay(input)
}

println input.length()