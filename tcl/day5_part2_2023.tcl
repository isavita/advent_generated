
proc reverse_convert_number {number ranges} {
    foreach r [lreverse $ranges] {
        if {[lindex $r 1] <= $number && $number < [expr {[lindex $r 1] + [lindex $r 2]}]} {
            return [expr {[lindex $r 0] + ($number - [lindex $r 1])}]
        }
    }
    return $number
}

proc is_in_seed_ranges {number seed_ranges} {
    foreach r $seed_ranges {
        if {[lindex $r 0] <= $number && $number < [expr {[lindex $r 0] + [lindex $r 1]}]} {
            return 1
        }
    }
    return 0
}

proc main {} {
    set file [open "input.txt" r]
    set lines [split [read $file] "\n"]
    close $file

    set seed_ranges {}
    set current_ranges {}
    set maps {}

    foreach line $lines {
        set line [string trim $line]
        if {[string match "*map:*" $line]} {
            if {[llength $current_ranges] > 0} {
                lappend maps $current_ranges
                set current_ranges {}
            }
        } elseif {[string match "seeds:*" $line]} {
            set seed_info [split [string range $line 7 end]]
            for {set i 0} {$i < [llength $seed_info]} {incr i 2} {
                set start [lindex $seed_info $i]
                set length [lindex $seed_info [expr {$i + 1}]]
                lappend seed_ranges [list $start $length]
            }
        } else {
            set numbers [split $line]
            if {[llength $numbers] == 3} {
                set destStart [lindex $numbers 0]
                set srcStart [lindex $numbers 1]
                set length [lindex $numbers 2]
                lappend current_ranges [list $srcStart $destStart $length]
            }
        }
    }

    if {[llength $current_ranges] > 0} {
        lappend maps $current_ranges
    }

    set location 0
    while 1 {
        set seed $location
        foreach m [lreverse $maps] {
            set seed [reverse_convert_number $seed $m]
        }

        if {[is_in_seed_ranges $seed $seed_ranges]} {
            puts $location
            break
        }

        incr location
    }
}

main
