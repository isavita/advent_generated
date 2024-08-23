proc main {} {
    set file [open "input.txt" r]
    set input [read $file]
    close $file

    set memory [split $input ","]
    set memory [map {expr {int($item)}} $memory]

    set memory [lreplace $memory 1 1 12]
    set memory [lreplace $memory 2 2 2]

    set i 0
    while {$i < [llength $memory]} {
        set opcode [lindex $memory $i]
        if {$opcode == 99} {
            break
        }
        set param1 [lindex $memory [expr {$i + 1}]]
        set param2 [lindex $memory [expr {$i + 2}]]
        set param3 [lindex $memory [expr {$i + 3}]]
        if {$opcode == 1} {
            set memory [lreplace $memory $param3 $param3 [expr {[lindex $memory $param1] + [lindex $memory $param2]}]]
        } elseif {$opcode == 2} {
            set memory [lreplace $memory $param3 $param3 [expr {[lindex $memory $param1] * [lindex $memory $param2]}]]
        }
        set i [expr {$i + 4}]
    }
    puts [lindex $memory 0]
}

proc map {func list} {
    set result {}
    foreach item $list {
        lappend result [eval $func]
    }
    return $result
}

main