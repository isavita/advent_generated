
proc hash {str} {
    set value 0
    foreach char [split $str ""] {
        set value [expr {($value + [scan $char "%c"]) * 17 % 256}]
    }
    return $value
}

proc part1 {steps} {
    set total 0
    foreach step $steps {
        set total [expr {$total + [hash $step]}]
    }
    return $total
}

proc part2 {steps} {
    set boxes [lrepeat 256 {}]
    foreach step $steps {
        if {[regexp {^([a-z]+)([-=])(\d*)$} $step -> label op focal]} {
            set box_num [hash $label]
            set box [lindex $boxes $box_num]
            
            if {$op eq "-"} {
                set new_box {}
                foreach lens $box {
                    if {[lindex $lens 0] ne $label} {
                        lappend new_box $lens
                    }
                }
                lset boxes $box_num $new_box
            } else {
                set found 0
                set new_box {}
                foreach lens $box {
                    if {[lindex $lens 0] eq $label} {
                        lappend new_box [list $label $focal]
                        set found 1
                    } else {
                        lappend new_box $lens
                    }
                }
                if {!$found} {
                    lappend new_box [list $label $focal]
                }
                lset boxes $box_num $new_box
            }
        }
    }
    
    set total_power 0
    for {set box_num 0} {$box_num < 256} {incr box_num} {
        set box [lindex $boxes $box_num]
        for {set slot 0} {$slot < [llength $box]} {incr slot} {
            set lens [lindex $box $slot]
            set focal_length [lindex $lens 1]
            set power [expr {($box_num + 1) * ($slot + 1) * $focal_length}]
            set total_power [expr {$total_power + $power}]
        }
    }
    return $total_power
}

# Read input from file
set file [open "input.txt" r]
set input [read $file]
close $file

# Remove newline and split into steps
set steps [split [string trim $input "\n"] ","]

# Part 1
set part1_result [part1 $steps]
puts "Part 1: $part1_result"

# Part 2
set part2_result [part2 $steps]
puts "Part 2: $part2_result"
