
proc calculate {nums ops} {
    set result [lindex $nums 0]
    for {set i 0} {$i < [llength $ops]} {incr i} {
        set op [lindex $ops $i]
        set num [lindex $nums $i+1]
        if {$op eq "+"} {
            set result [expr {$result + $num}]
        } else {
            set result [expr {$result * $num}]
        }
    }
    return $result
}

proc solve_equation {line} {
    regexp {^(\d+):\s+(.*)$} $line -> target nums_str
    set target [string trim $target]
    set nums [lmap num $nums_str {string trim $num}]
    
    set num_count [llength $nums]
    if {$num_count <= 1} {
        return 0
    }
    
    set op_count [expr {$num_count - 1}]
    
    for {set i 0} {$i < [expr {pow(2, $op_count)}]} {incr i} {
        set ops ""
        set temp $i
        for {set j 0} {$j < $op_count} {incr j} {
            if {$temp % 2 == 0} {
                lappend ops "+"
            } else {
                lappend ops "*"
            }
            set temp [expr {$temp / 2}]
        }
        
        if {[calculate $nums $ops] == $target} {
            return $target
        }
    }
    return 0
}

set total_sum 0
if {[file exists "input.txt"]} {
    set file [open "input.txt" r]
    while {[gets $file line] != -1} {
        set total_sum [expr {$total_sum + [solve_equation $line]}]
    }
    close $file
} else {
    puts "Error: input.txt not found"
    exit 1
}

puts $total_sum
