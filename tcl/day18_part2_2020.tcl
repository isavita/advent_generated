proc solve {input} {
    set lines [split $input "\n"]
    set total 0
    foreach line $lines {
        set total [expr {$total + [doMaths [parseInput $line]]}]
    }
    return $total
}

proc parseInput {line} {
    return [split [string trim [string map {" " ""} $line]] ""]
}

proc doMaths {input} {
    set stackOpenIndices {}
    set stackFlattened {}
    foreach item $input {
        lappend stackFlattened $item
        if {$item == "("} {
            lappend stackOpenIndices [expr {[llength $stackFlattened] - 1}]
        } elseif {$item == ")"} {
            set openIndex [lindex $stackOpenIndices end]
            set stackOpenIndices [lrange $stackOpenIndices 0 end-1]
            set sliToFlatten [lrange $stackFlattened [expr {$openIndex + 1}] end-1]
            set stackFlattened [lreplace $stackFlattened $openIndex end [calcFlatSlicePart $sliToFlatten]]
        }
    }
    return [toInt [calcFlatSlicePart $stackFlattened]]
}

proc calcFlatSlicePart {input} {
    for {set i 1} {$i < [llength $input] - 1} {incr i} {
        if {[lindex $input $i] == "+"} {
            set toLeft [lindex $input [expr {$i - 1}]]
            set toRight [lindex $input [expr {$i + 1}]]
            if {[isNum $toLeft] && [isNum $toRight]} {
                set input [lreplace $input [expr {$i - 1}] [expr {$i + 1}] [addStrings $toLeft $toRight]]
                set i [expr {$i - 1}]
            }
        }
    }
    for {set i 1} {$i < [llength $input] - 1} {incr i} {
        if {[lindex $input $i] == "*"} {
            set toLeft [lindex $input [expr {$i - 1}]]
            set toRight [lindex $input [expr {$i + 1}]]
            if {[isNum $toLeft] && [isNum $toRight]} {
                set input [lreplace $input [expr {$i - 1}] [expr {$i + 1}] [multiplyStrings $toLeft $toRight]]
                set i [expr {$i - 1}]
            }
        }
    }
    return [lindex $input 0]
}

proc isNum {str} {
    return [regexp {^\d+$} $str]
}

proc addStrings {args} {
    set sum 0
    foreach str $args {
        set sum [expr {$sum + [toInt $str]}]
    }
    return $sum
}

proc multiplyStrings {args} {
    set product 1
    foreach str $args {
        set product [expr {$product * [toInt $str]}]
    }
    return $product
}

proc toInt {s} {
    return [expr {$s + 0}]
}

set input [read [open "input.txt"]]
set ans [solve $input]
puts $ans