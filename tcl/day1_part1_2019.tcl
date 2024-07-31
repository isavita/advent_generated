set masses {}
set total 0.0

proc processLine {line} {
    global masses
    lappend masses [expr {int([string trim $line])}]
}

proc getTotal {} {
    global masses total
    foreach m $masses {
        set total [expr {$total + floor($m / 3) - 2}]
    }
}

set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    processLine $line
}
close $file

getTotal
puts $total