set file [open "input.txt" r]
set program [split [gets $file] ","]
close $file
set program [lmap x $program {expr {$x}}]
set input 5
set output 0
set i 0

proc getValue {program pos mode} {
    if {$mode == 0} {
        return [lindex $program [lindex $program $pos]]
    } else {
        return [lindex $program $pos]
    }
}

while {1} {
    set opcode [expr {[lindex $program $i] % 100}]
    set modes [expr {[lindex $program $i] / 100}]
    set param1Mode [expr {$modes % 10}]
    set modes [expr {$modes / 10}]
    set param2Mode [expr {$modes % 10}]

    switch $opcode {
        1 {
            set p1 [getValue $program [expr {$i + 1}] $param1Mode]
            set p2 [getValue $program [expr {$i + 2}] $param2Mode]
            set p3 [lindex $program [expr {$i + 3}]]
            set program [lreplace $program $p3 $p3 [expr {$p1 + $p2}]]
            set i [expr {$i + 4}]
        }
        2 {
            set p1 [getValue $program [expr {$i + 1}] $param1Mode]
            set p2 [getValue $program [expr {$i + 2}] $param2Mode]
            set p3 [lindex $program [expr {$i + 3}]]
            set program [lreplace $program $p3 $p3 [expr {$p1 * $p2}]]
            set i [expr {$i + 4}]
        }
        3 {
            set program [lreplace $program [lindex $program [expr {$i + 1}]] [lindex $program [expr {$i + 1}]] $input]
            set i [expr {$i + 2}]
        }
        4 {
            set output [getValue $program [expr {$i + 1}] $param1Mode]
            puts $output
            set i [expr {$i + 2}]
        }
        5 {
            set p1 [getValue $program [expr {$i + 1}] $param1Mode]
            set p2 [getValue $program [expr {$i + 2}] $param2Mode]
            if {$p1 != 0} {
                set i $p2
            } else {
                set i [expr {$i + 3}]
            }
        }
        6 {
            set p1 [getValue $program [expr {$i + 1}] $param1Mode]
            set p2 [getValue $program [expr {$i + 2}] $param2Mode]
            if {$p1 == 0} {
                set i $p2
            } else {
                set i [expr {$i + 3}]
            }
        }
        7 {
            set p1 [getValue $program [expr {$i + 1}] $param1Mode]
            set p2 [getValue $program [expr {$i + 2}] $param2Mode]
            set p3 [lindex $program [expr {$i + 3}]]
            set program [lreplace $program $p3 $p3 [expr {$p1 < $p2 ? 1 : 0}]]
            set i [expr {$i + 4}]
        }
        8 {
            set p1 [getValue $program [expr {$i + 1}] $param1Mode]
            set p2 [getValue $program [expr {$i + 2}] $param2Mode]
            set p3 [lindex $program [expr {$i + 3}]]
            set program [lreplace $program $p3 $p3 [expr {$p1 == $p2 ? 1 : 0}]]
            set i [expr {$i + 4}]
        }
        99 {
            break
        }
        default {
            error "Invalid opcode"
        }
    }
}