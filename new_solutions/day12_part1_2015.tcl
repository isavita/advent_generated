set file [open "input.txt" r]
set jsonString [read $file]
close $file

proc sumNumbers {jsonString} {
    set sum 0
    set number ""
    foreach char [split $jsonString ""] {
        if {[string is digit $char] || $char == "-" || $char == "."} {
            append number $char
        } else {
            if {$number ne ""} {
                set sum [expr {$sum + $number}]
                set number ""
            }
        }
    }
    if {$number ne ""} {
        set sum [expr {$sum + $number}]
    }
    return $sum
}

puts [sumNumbers $jsonString]