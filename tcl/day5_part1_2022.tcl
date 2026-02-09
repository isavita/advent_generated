
set fp [open input.txt r]
set stacks {}

# Parse initial state
while {[gets $fp line] >= 0} {
    if {[string trim $line] eq ""} break
    for {set i 1} {$i < [string length $line]} {incr i 4} {
        set char [string index $line $i]
        if {[string is alpha $char]} {
            set idx [expr {($i - 1) / 4}]
            # Ensure stack exists
            while {[llength $stacks] <= $idx} {
                lappend stacks {}
            }
            # Append to end (top of logical stack in reading order)
            lset stacks $idx [linsert [lindex $stacks $idx] end $char]
        }
    }
}

# Reverse stacks to correct order (bottom to top)
for {set i 0} {$i < [llength $stacks]} {incr i} {
    lset stacks $i [lreverse [lindex $stacks $i]]
}

# Process moves
while {[gets $fp line] >= 0} {
    if {[regexp {move (\d+) from (\d+) to (\d+)} $line -> n src dst]} {
        incr src -1
        incr dst -1
        for {set i 0} {$i < $n} {incr i} {
            set item [lindex [lindex $stacks $src] end]
            lset stacks $src [lreplace [lindex $stacks $src] end end]
            lset stacks $dst [linsert [lindex $stacks $dst] end $item]
        }
    }
}
close $fp

# Output result
set result ""
foreach s $stacks {
    append result [lindex $s end]
}
puts $result
