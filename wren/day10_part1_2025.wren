
import "io" for File

var contents = File.read("input.txt")
var lines = contents.replace("\r", "").split("\n")
var total = 0

for (line in lines) {
    var startBrack = line.indexOf("[")
    if (startBrack == -1) continue
    var endBrack = line.indexOf("]")
    if (endBrack == -1) continue
    var targetStr = line[startBrack + 1...endBrack]
    var R = targetStr.count
    var b = List.filled(R, 0)
    for (i in 0...R) {
        if (targetStr[i] == "#") b[i] = 1
    }
    
    var s = line[endBrack + 1..-1]
    var buttons = []
    var inParen = false
    var currentButton = []
    var numStr = ""
    for (i in 0...s.count) {
        var char = s[i]
        if (char == "(") {
            inParen = true
            currentButton = []
            numStr = ""
        } else if (char == ")") {
            if (numStr != "") {
                currentButton.add(Num.fromString(numStr))
                numStr = ""
            }
            buttons.add(currentButton)
            inParen = false
        } else if (inParen) {
            if (char == "," || char == " ") {
                if (numStr != "") {
                    currentButton.add(Num.fromString(numStr))
                    numStr = ""
                }
            } else if ("0123456789".contains(char)) {
                numStr = numStr + char
            }
        }
    }
    
    var C = buttons.count
    var mat = List.filled(R, null)
    for (i in 0...R) {
        mat[i] = List.filled(C + 1, 0)
        mat[i][C] = b[i]
    }
    for (j in 0...C) {
        for (light in buttons[j]) {
            mat[light][j] = 1
        }
    }
    
    var pRow = 0
    var isPivot = List.filled(C, false)
    for (j in 0...C) {
        if (pRow >= R) break
        var sel = -1
        for (i in pRow...R) {
            if (mat[i][j] == 1) {
                sel = i
                break
            }
        }
        if (sel == -1) continue
        if (sel != pRow) {
            var temp = mat[pRow]
            mat[pRow] = mat[sel]
            mat[sel] = temp
        }
        for (i in 0...R) {
            if (i != pRow && mat[i][j] == 1) {
                for (k in j..C) {
                    mat[i][k] = mat[i][k] ^ mat[pRow][k]
                }
            }
        }
        isPivot[j] = true
        pRow = pRow + 1
    }
    
    var solvable = true
    for (i in pRow...R) {
        if (mat[i][C] == 1) {
            solvable = false
            break
        }
    }
    if (!solvable) continue
    
    var freeVars = []
    for (j in 0...C) {
        if (!isPivot[j]) freeVars.add(j)
    }
    
    var nFree = freeVars.count
    var minW = 1e9
    var limit = 1
    for (i in 0...nFree) limit = limit * 2
    var powers = List.filled(nFree, 1)
    if (nFree > 0) {
        for (f in 1...nFree) {
            powers[f] = powers[f-1] * 2
        }
    }
    
    var x = List.filled(C, 0)
    for (i in 0...limit) {
        var cw = 0
        for (f in 0...nFree) {
            var val = ((i / powers[f]).floor) % 2
            x[freeVars[f]] = val
            if (val == 1) cw = cw + 1
        }
        
        var currPR = 0
        for (j in 0...C) {
            if (isPivot[j]) {
                var val = mat[currPR][C]
                for (k in (j + 1)...C) {
                    if (mat[currPR][k] == 1 && x[k] == 1) {
                        val = 1 - val
                    }
                }
                x[j] = val
                if (val == 1) cw = cw + 1
                currPR = currPR + 1
            }
        }
        if (cw < minW) minW = cw
    }
    if (minW != 1e9) total = total + minW
}

System.print(total)
