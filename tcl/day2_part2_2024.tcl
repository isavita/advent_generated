
proc is_safe {levels} {
    set len [llength $levels]
    if {$len <= 1} {
        return 1
    }

    set dir ""
    for {set i 0} {$i < $len - 1} {incr i} {
        set diff [expr {[lindex $levels $i+1] - [lindex $levels $i]}]
        if {$diff == 0} {
            return 0
        }
        if {$dir eq ""} {
            if {$diff > 0} {
                set dir "inc"
            } else {
                set dir "dec"
            }
        } else {
            if {$dir eq "inc" && $diff < 0} {
                return 0
            } elseif {$dir eq "dec" && $diff > 0} {
                return 0
            }
        }
        if {abs($diff) > 3} {
            return 0
        }
    }
    return 1
}

proc is_safe_with_dampener {levels} {
    if {[is_safe $levels]} {
        return 1
    }
    set len [llength $levels]
    for {set i 0} {$i < $len} {incr i} {
        set temp_levels [lreplace $levels $i $i]
        if {[is_safe $temp_levels]} {
            return 1
        }
    }
    return 0
}

set safe_count 0
if {[catch {open "input.txt" r} file_id]} {
    puts "Error opening input.txt: $file_id"
    exit 1
}

while {[gets $file_id line] != -1} {
    set levels [split $line]
    if {[is_safe_with_dampener $levels]} {
        incr safe_count
    }
}
close $file_id
puts $safe_count
