set file [open "input.txt" r]
set lines [split [read $file] \n]
close $file

set found ""
for {set i 0} {$i < [llength $lines]-1} {incr i} {
    for {set j [expr {$i + 1}]} {$j < [llength $lines]} {incr j} {
        set diff 0
        for {set k 0} {$k < [string length [lindex $lines $i]]} {incr k} {
            if {[string index [lindex $lines $i] $k] ne [string index [lindex $lines $j] $k]} {
                incr diff
                if {$diff > 1} break
            }
        }
        if {$diff == 1} {
            set common ""
            for {set k 0} {$k < [string length [lindex $lines $i]]} {incr k} {
                if {[string index [lindex $lines $i] $k] eq [string index [lindex $lines $j] $k]} {
                    append common [string index [lindex $lines $i] $k]
                }
            }
            puts $common
            return
        }
    }
}