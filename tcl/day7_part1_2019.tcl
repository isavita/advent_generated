proc readInput {filename} {
    set file [open $filename r]
    set data [split [read $file] ","]
    close $file
    return [lmap x $data {expr {$x}}]
}

proc runProgram {program inputs} {
    set memory [lassign $program]
    set inputIndex 0
    set output 0
    set pc 0

    while {$pc < [llength $memory]} {
        set opcode [expr {[lindex $memory $pc] % 100}]
        set modes [expr {[lindex $memory $pc] / 100}]
        switch -- $opcode {
            1 {
                set p1 [getValue $memory $pc 1 $modes]
                set p2 [getValue $memory $pc 2 $modes]
                set dest [lindex $memory [expr {$pc + 3}]]
                set memory [replace $memory $dest [expr {$p1 + $p2}]]
                set pc [expr {$pc + 4}]
            }
            2 {
                set p1 [getValue $memory $pc 1 $modes]
                set p2 [getValue $memory $pc 2 $modes]
                set dest [lindex $memory [expr {$pc + 3}]]
                set memory [replace $memory $dest [expr {$p1 * $p2}]]
                set pc [expr {$pc + 4}]
            }
            3 {
                set dest [lindex $memory [expr {$pc + 1}]]
                set memory [replace $memory $dest [lindex $inputs $inputIndex]]
                incr inputIndex
                set pc [expr {$pc + 2}]
            }
            4 {
                set output [getValue $memory $pc 1 $modes]
                set pc [expr {$pc + 2}]
            }
            5 {
                set p1 [getValue $memory $pc 1 $modes]
                set p2 [getValue $memory $pc 2 $modes]
                if {$p1 != 0} {set pc $p2} else {set pc [expr {$pc + 3}]}
            }
            6 {
                set p1 [getValue $memory $pc 1 $modes]
                set p2 [getValue $memory $pc 2 $modes]
                if {$p1 == 0} {set pc $p2} else {set pc [expr {$pc + 3}]}
            }
            7 {
                set p1 [getValue $memory $pc 1 $modes]
                set p2 [getValue $memory $pc 2 $modes]
                set dest [lindex $memory [expr {$pc + 3}]]
                set memory [replace $memory $dest [expr {$p1 < $p2 ? 1 : 0}]]
                set pc [expr {$pc + 4}]
            }
            8 {
                set p1 [getValue $memory $pc 1 $modes]
                set p2 [getValue $memory $pc 2 $modes]
                set dest [lindex $memory [expr {$pc + 3}]]
                set memory [replace $memory $dest [expr {$p1 == $p2 ? 1 : 0}]]
                set pc [expr {$pc + 4}]
            }
            99 {break}
            default {error "Unknown opcode $opcode"}
        }
    }
    return $output
}

proc getValue {memory pc param modes} {
    set mode [expr {($modes / [expr {10 ** ($param - 1)}]) % 10}]
    set value [lindex $memory [expr {$pc + $param}]]
    return [expr {$mode == 0 ? [lindex $memory $value] : $value}]
}

proc replace {list index value} {
    set list [lreplace $list $index $index $value]
    return $list
}

proc permute {list} {
    if {[llength $list] <= 1} {return [list $list]}
    set result {}
    foreach i $list {
        set rest [lreplace $list [lsearch -exact $list $i] [lsearch -exact $list $i]]
        foreach p [permute $rest] {lappend result [linsert $p 0 $i]}
    }
    return $result
}

proc maxThrusterSignal {program} {
    set maxSignal 0
    foreach phases [permute {0 1 2 3 4}] {
        set signal 0
        foreach phase $phases {
            set signal [runProgram $program [list $phase $signal]]
        }
        if {$signal > $maxSignal} {set maxSignal $signal}
    }
    return $maxSignal
}

set program [readInput "input.txt"]
puts [maxThrusterSignal $program]