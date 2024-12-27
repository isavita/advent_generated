
set f [open "input.txt" r]
set line [string trim [read $f]]
close $f

set disk {}
set fileID 0
set isFile 1
foreach char [split $line ""] {
    if {$char eq ""} continue
    set length [string trim $char]
    if {$isFile} {
        for {set j 0} {$j < $length} {incr j} {
            lappend disk $fileID
        }
        incr fileID
    } else {
        for {set j 0} {$j < $length} {incr j} {
            lappend disk "."
        }
    }
    set isFile [expr !$isFile]
}

set files {}
set curID -1
set start 0
for {set i 0} {$i < [llength $disk]} {incr i} {
    set b [lindex $disk $i]
    if {$b eq "."} {
        set curID -1
        continue
    }
    if {$b ne $curID} {
        set curID $b
        set start $i
    }
    if {$i == [llength $disk]-1 || ([lindex $disk [expr $i+1]] ne $b)} {
        lappend files [list $b $start $i]
    }
}

for {set i [expr [llength $files] - 1]} {$i >= 0} {incr i -1} {
    set f [lindex $files $i]
    lassign $f id start end
    set fileLen [expr $end - $start + 1]
    set leftmostSpan -1
    set spanLen 0
    for {set j 0} {$j < $start} {incr j} {
        if {[lindex $disk $j] eq "."} {
            if {$spanLen == 0} {
                set leftmostSpan $j
            }
            incr spanLen
            if {$spanLen == $fileLen} {
                break
            }
        } else {
            set spanLen 0
            set leftmostSpan -1
        }
    }
    if {$leftmostSpan != -1 && $spanLen == $fileLen} {
        for {set x $start} {$x <= $end} {incr x} {
            lset disk $x "."
        }
        for {set x 0} {$x < $fileLen} {incr x} {
            lset disk [expr $leftmostSpan + $x] $id
        }
    }
}

set checksum 0
for {set i 0} {$i < [llength $disk]} {incr i} {
    set b [lindex $disk $i]
    if {$b ne "."} {
        set checksum [expr $checksum + $i * $b]
    }
}
puts $checksum
