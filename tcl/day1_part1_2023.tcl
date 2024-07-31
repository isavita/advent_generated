set sum 0
set file [open "input.txt" r]

while {[gets $file line] >= 0} {
    set firstDigit -1
    set lastDigit -1
    foreach r [split $line ""] {
        if {[string is digit $r]} {
            if {$firstDigit == -1} {
                set firstDigit $r
            }
            set lastDigit $r
        }
    }
    if {$firstDigit != -1 && $lastDigit != -1} {
        set value [expr {$firstDigit * 10 + $lastDigit}]
        set sum [expr {$sum + $value}]
    }
}

close $file
puts $sum