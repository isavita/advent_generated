proc readInstructions {filename} {
    set file [open $filename r]
    set instructions [list]
    while {[gets $file line] >= 0} {
        lappend instructions $line
    }
    close $file
    return $instructions
}

proc executeProgram {instructions} {
    set accumulator 0
    array set visited {}
    set pc 0
    while {$pc < [llength $instructions]} {
        if {[info exists visited($pc)]} {
            return -1
        }
        set visited($pc) 1
        set parts [split [lindex $instructions $pc] " "]
        set op [lindex $parts 0]
        set arg [expr {[lindex $parts 1]}]
        switch -- $op {
            acc {set accumulator [expr {$accumulator + $arg}]}
            jmp {set pc [expr {$pc + $arg - 1}]}
        }
        incr pc
    }
    return $accumulator
}

proc fixProgram {instructions} {
    for {set i 0} {$i < [llength $instructions]} {incr i} {
        set parts [split [lindex $instructions $i] " "]
        set op [lindex $parts 0]
        if {$op eq "nop" || $op eq "jmp"} {
            set modified [lreplace $instructions $i $i [join [list [expr {$op eq "nop" ? "jmp" : "nop"}] [lindex $parts 1]] " "]]
            set result [executeProgram $modified]
            if {$result != -1} {
                return $result
            }
        }
    }
    return -1
}

set instructions [readInstructions "input.txt"]
puts [fixProgram $instructions]