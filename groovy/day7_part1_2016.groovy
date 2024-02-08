
def input = new File("input.txt").text.split("\n")

def supportsTLS = { ip ->
    def inHypernet = false
    def abbaOutside = false
    def abbaInside = false
    
    for (int i = 0; i < ip.size() - 3; i++) {
        def fourChars = ip[i..i+3]
        
        if (fourChars[0] == fourChars[3] && fourChars[1] == fourChars[2] && fourChars[0] != fourChars[1]) {
            if (inHypernet) {
                abbaInside = true
            } else {
                abbaOutside = true
            }
        }
        
        if (fourChars[0] == '[') {
            inHypernet = true
        }
        
        if (fourChars[0] == ']') {
            inHypernet = false
        }
    }
    
    abbaOutside && !abbaInside
}

def count = input.findAll { supportsTLS(it) }.size()

println count
