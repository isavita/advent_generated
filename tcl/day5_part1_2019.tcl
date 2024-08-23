proc readFile {filename} {
    set file [open $filename r]
    set content [read $file]
    close $file
    return $content
}

proc parseInput {input} {
    return [split $input ","]
}

proc getParameter {program index mode} {
    if {$mode == 0} {
        return [lindex $program [lindex $program $index]]
    } else {
        return [lindex $program $index]
    }
}

proc runIntcode {program} {
    set i 0
    set input 1
    set output 0

    while {$i < [llength $program]} {
        set opcode [lindex $program $i]
        set op [expr {$opcode % 100}]
        set mode1 [expr {($opcode / 100) % 10}]
        set mode2 [expr {($opcode / 1000) % 10}]

        switch -- $op {
            1 {
                set param1 [getParameter $program [expr {$i + 1}] $mode1]
                set param2 [getParameter $program [expr {$i + 2}] $mode2]
                set dest [lindex $program [expr {$i + 3}]]
                set program [lreplace $program $dest $dest [expr {$param1 + $param2}]]
                set i [expr {$i + 4}]
            }
            2 {
                set param1 [getParameter $program [expr {$i + 1}] $mode1]
                set param2 [getParameter $program [expr {$i + 2}] $mode2]
                set dest [lindex $program [expr {$i + 3}]]
                set program [lreplace $program $dest $dest [expr {$param1 * $param2}]]
                set i [expr {$i + 4}]
            }
            3 {
                set dest [lindex $program [expr {$i + 1}]]
                set program [lreplace $program $dest $dest $input]
                set i [expr {$i + 2}]
            }
            4 {
                set output [getParameter $program [expr {$i + 1}] $mode1]
                puts $output
                set i [expr {$i + 2}]
            }
            99 {
                break
            }
            default {
                error "Unknown opcode: $op"
            }
        }
    }
    return $output
}

proc convertToIntList {list} {
    set result {}
    foreach item $list {
        lappend result [expr {$item}]
    }
    return $result
}

set input [readFile "input.txt"]
set program [parseInput $input]
set program [convertToIntList $program]
runIntcode $program