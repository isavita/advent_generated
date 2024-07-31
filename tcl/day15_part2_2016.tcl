set file [open "input.txt" r]
set discs {}

while {[gets $file line] >= 0} {
    regexp {Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).} $line -> _ totalPositions startPosition
    lappend discs [list $totalPositions $startPosition]
}
close $file

lappend discs [list 11 0]

set time 0
while {1} {
    set valid 1
    for {set i 0} {$i < [llength $discs]} {incr i} {
        set disc [lindex $discs $i]
        set position [expr {([lindex $disc 1] + $time + $i + 1) % [lindex $disc 0]}]
        if {$position != 0} {
            set valid 0
            break
        }
    }
    if {$valid} {
        puts $time
        break
    }
    incr time
}