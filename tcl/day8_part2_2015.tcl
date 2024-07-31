set file [open "input.txt" r]
set totalDiff 0

proc calculateEncodedLength {s} {
    set encoded "\""
    foreach ch [split $s ""] {
        if {$ch eq "\\" || $ch eq "\""} {
            append encoded "\\"
        }
        append encoded $ch
    }
    append encoded "\""
    return [string length $encoded]
}

while {[gets $file line] >= 0} {
    set originalLength [string length $line]
    set encodedLength [calculateEncodedLength $line]
    set totalDiff [expr {$totalDiff + $encodedLength - $originalLength}]
}

close $file
puts $totalDiff