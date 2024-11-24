
#!/usr/bin/env tclsh

proc strToInt {s} {
    return [expr {int($s)}]
}

proc regSplit {text delimiter} {
    set result {}
    set regex [subst -nocommands {$delimiter}]
    set matches [regexp -all -inline $regex $text]
    set lastIndex 0
    
    foreach match $matches {
        set start [string first $match $text $lastIndex]
        if {$start != -1} {
            lappend result [string range $text $lastIndex [expr {$start - 1}]]
            set lastIndex [expr {$start + [string length $match]}]
        }
    }
    
    lappend result [string range $text $lastIndex end]
    return $result
}

proc runOp {op registers instruction} {
    set registerCP [lrange $registers 0 end]
    
    # Determine A value
    if {[lindex $op 1] eq "r"} {
        set A [lindex $registerCP [lindex $instruction 1]]
    } else {
        set A [lindex $instruction 1]
    }
    
    # Determine B value
    if {[lindex $op 2] eq "r"} {
        set B [lindex $registerCP [lindex $instruction 2]]
    } else {
        set B [lindex $instruction 2]
    }
    
    # Perform operation
    switch [lindex $op 0] {
        "+" { lset registerCP [lindex $instruction 3] [expr {$A + $B}] }
        "*" { lset registerCP [lindex $instruction 3] [expr {$A * $B}] }
        "&" { lset registerCP [lindex $instruction 3] [expr {$A & $B}] }
        "|" { lset registerCP [lindex $instruction 3] [expr {$A | $B}] }
        "a" { lset registerCP [lindex $instruction 3] $A }
        ">" { lset registerCP [lindex $instruction 3] [expr {$A > $B ? 1 : 0}] }
        "=" { lset registerCP [lindex $instruction 3] [expr {$A == $B ? 1 : 0}] }
    }
    
    return $registerCP
}

proc match {r c} {
    if {[llength $r] != [llength $c]} {
        return 0
    }
    
    for {set i 0} {$i < [llength $r]} {incr i} {
        if {[lindex $r $i] != [lindex $c $i]} {
            return 0
        }
    }
    
    return 1
}

proc testCode {registers n instruction opcodes} {
    set sum 0
    
    for {set i 0} {$i < [llength $opcodes]} {incr i} {
        set result [runOp [lindex $opcodes $i] $registers $instruction]
        
        if {[match $n $result]} {
            incr sum
        }
    }
    
    return $sum
}

# Main program
set input [read [open "input.txt" r]]
set input [string trim $input]
set lines [split $input "\n"]

set opcodes [list \
    [list "+" "r" "r" "addr"] \
    [list "+" "r" "v" "addi"] \
    [list "*" "r" "r" "mulr"] \
    [list "*" "r" "v" "muli"] \
    [list "&" "r" "r" "banr"] \
    [list "&" "r" "v" "bani"] \
    [list "|" "r" "r" "borr"] \
    [list "|" "r" "v" "bori"] \
    [list "a" "r" "r" "setr"] \
    [list "a" "v" "r" "seti"] \
    [list ">" "v" "r" "gtir"] \
    [list ">" "r" "v" "gtri"] \
    [list ">" "r" "r" "gtrr"] \
    [list "=" "v" "r" "eqir"] \
    [list "=" "r" "v" "eqri"] \
    [list "=" "r" "r" "eqrr"] \
]

set sum 0
set lineCount 0

while {$lineCount < [llength $lines]} {
    if {[string length [lindex $lines $lineCount]] > 0 && [string index [lindex $lines $lineCount] 0] eq "B"} {
        set split [regSplit [lindex $lines $lineCount] {[^0-9]+}]
        set registers [list \
            [strToInt [lindex $split 1]] \
            [strToInt [lindex $split 2]] \
            [strToInt [lindex $split 3]] \
            [strToInt [lindex $split 4]] \
        ]
        
        set split [regSplit [lindex $lines [expr {$lineCount + 1}]] {[^0-9]+}]
        set instruction [list \
            [strToInt [lindex $split 0]] \
            [strToInt [lindex $split 1]] \
            [strToInt [lindex $split 2]] \
            [strToInt [lindex $split 3]] \
        ]
        
        set split [regSplit [lindex $lines [expr {$lineCount + 2}]] {[^0-9]+}]
        set n [list \
            [strToInt [lindex $split 1]] \
            [strToInt [lindex $split 2]] \
            [strToInt [lindex $split 3]] \
            [strToInt [lindex $split 4]] \
        ]
        
        set tempSum [testCode $registers $n $instruction $opcodes]
        
        if {$tempSum >= 3} {
            incr sum
        }
        
        incr lineCount 4
    } else {
        break
    }
}

puts $sum
