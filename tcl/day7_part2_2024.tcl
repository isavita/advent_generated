
proc calculate {values ops} {
    set result [lindex $values 0]
    for {set i 0} {$i < [llength $ops]} {incr i} {
        set op [lindex $ops $i]
        set next_val [lindex $values [expr {$i + 1}]]
        switch $op {
            "+" {set result [expr {$result + $next_val}]}
            "*" {set result [expr {$result * $next_val}]}
            "||" {set result [string cat $result $next_val]}
        }
    }
    return $result
}

proc solve_equation {target values} {
    set num_values [llength $values]
    if {$num_values == 1} {
        return [expr {[lindex $values 0] == $target}]
    }
    
    set num_ops [expr {$num_values - 1}]
    
    for {set i 0} {$i < [expr {3**$num_ops}]} {incr i} {
        set temp $i
        set ops {}
        for {set j 0} {$j < $num_ops} {incr j} {
            set op_index [expr {$temp % 3}]
            switch $op_index {
                0 {lappend ops "+"}
                1 {lappend ops "*"}
                2 {lappend ops "||"}
            }
            set temp [expr {$temp / 3}]
        }
        
        if {[calculate $values $ops] == $target} {
            return 1
        }
    }
    return 0
}

set total_sum 0
set file [open "input.txt" r]
while {[gets $file line] != -1} {
    if {[regexp {^(\d+):\s+(.*)$} $line match target values_str]} {
        set values [split $values_str]
        if {[solve_equation $target $values]} {
            set total_sum [expr {$total_sum + $target}]
        }
    }
}
close $file
puts $total_sum
