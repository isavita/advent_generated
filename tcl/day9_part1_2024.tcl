
proc solve {} {
    set f [open "input.txt" r]
    gets $f line
    close $f

    set disk {}
    set file_id 0
    set is_file 1

    foreach char [split $line ""] {
        if {$char eq ""} continue
        set length [string trim $char]
        if {$is_file} {
            for {set i 0} {$i < $length} {incr i} {
                lappend disk $file_id
            }
            incr file_id
        } else {
            for {set i 0} {$i < $length} {incr i} {
                lappend disk "."
            }
        }
        set is_file [expr {1 - $is_file}]
    }

    while 1 {
        set lfree -1
        for {set i 0} {$i < [llength $disk]} {incr i} {
            if {[lindex $disk $i] eq "."} {
                set lfree $i
                break
            }
        }
        if {$lfree eq -1} break

        set rfile -1
        for {set i [expr {[llength $disk] - 1}]} {$i > $lfree} {incr i -1} {
            if {[lindex $disk $i] ne "."} {
                set rfile $i
                break
            }
        }
        if {$rfile eq -1} break

        lset disk $lfree [lindex $disk $rfile]
        lset disk $rfile "."
    }

    set checksum 0
    for {set i 0} {$i < [llength $disk]} {incr i} {
        set val [lindex $disk $i]
        if {$val ne "."} {
            incr checksum [expr {$i * $val}]
        }
    }
    puts $checksum
}

solve
