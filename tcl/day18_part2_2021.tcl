proc parse_snail_number {input_str} {
    set input_str [string trim $input_str]
    if {[string index $input_str 0] ne "\["} {
        return [list value [expr {int($input_str)}]]
    }

    set balance 0
    set split_index 0
    set len [string length $input_str]
    for {set i 1} {$i < $len-1} {incr i} {
        set char [string index $input_str $i]
        if {$char eq "\["} {
            incr balance
        } elseif {$char eq "\]"} {
            incr balance -1
        } elseif {$char eq "," && $balance == 0} {
            set split_index $i
            break
        }
    }

    set left [parse_snail_number [string range $input_str 1 [expr {$split_index-1}]]]
    set right [parse_snail_number [string range $input_str [expr {$split_index+1}] end-1]]
    return [list left $left right $right]
}

proc is_regular {num} {
    return [expr {[dict exists $num value]}]
}

proc add_left {num value} {
    if {[is_regular $num]} {
        dict set num value [expr {[dict get $num value] + $value}]
    } else {
        dict set num left [add_left [dict get $num left] $value]
    }
    return $num
}

proc add_right {num value} {
    if {[is_regular $num]} {
        dict set num value [expr {[dict get $num value] + $value}]
    } else {
        dict set num right [add_right [dict get $num right] $value]
    }
    return $num
}

proc explode {num depth} {
    if {[is_regular $num]} {
        return [list 0 0 0 $num]
    }

    if {$depth == 4} {
        set left_value [dict get [dict get $num left] value]
        set right_value [dict get [dict get $num right] value]
        set num [dict create value 0]
        return [list 1 $left_value $right_value $num]
    }

    lassign [explode [dict get $num left] [expr {$depth + 1}]] exploded left_val right_val new_left
    if {$exploded} {
        set num [dict set num left $new_left]
        if {$right_val > 0} {
            set num [dict set num right [add_left [dict get $num right] $right_val]]
        }
        return [list 1 $left_val 0 $num]
    }

    lassign [explode [dict get $num right] [expr {$depth + 1}]] exploded left_val right_val new_right
    if {$exploded} {
        set num [dict set num right $new_right]
        if {$left_val > 0} {
            set num [dict set num left [add_right [dict get $num left] $left_val]]
        }
        return [list 1 0 $right_val $num]
    }

    return [list 0 0 0 $num]
}

proc split {num} {
    if {[is_regular $num]} {
        set val [dict get $num value]
        if {$val >= 10} {
            set left_val [expr {$val / 2}]
            set right_val [expr {($val + 1) / 2}]
            return [list 1 [dict create left [dict create value $left_val] right [dict create value $right_val]]]
        }
        return [list 0 $num]
    }

    lassign [split [dict get $num left]] changed new_left
    if {$changed} {
        return [list 1 [dict set num left $new_left]]
    }

    lassign [split [dict get $num right]] changed new_right
    if {$changed} {
        return [list 1 [dict set num right $new_right]]
    }

    return [list 0 $num]
}

proc reduce {num} {
    while {1} {
        lassign [explode $num 0] exploded left_val right_val new_num
        set num $new_num
        if {$exploded} {
            continue
        }
        lassign [split $num] changed new_num
        set num $new_num
        if {!$changed} {
            break
        }
    }
    return $num
}

proc add {a b} {
    return [reduce [dict create left $a right $b]]
}

proc magnitude {num} {
    if {[is_regular $num]} {
        return [dict get $num value]
    }
    return [expr {3 * [magnitude [dict get $num left]] + 2 * [magnitude [dict get $num right]]}]
}

proc deep_copy {num} {
    if {[is_regular $num]} {
        return [dict create value [dict get $num value]]
    }
    return [dict create left [deep_copy [dict get $num left]] right [deep_copy [dict get $num right]]]
}

proc main {} {
    set file [open "input.txt" r]
    set snail_numbers {}
    while {[gets $file line] >= 0} {
        lappend snail_numbers [parse_snail_number $line]
    }
    close $file

    if {[llength $snail_numbers] == 0} {
        puts "No snailfish numbers found in the file."
        return
    }

    set largest_magnitude 0
    set n [llength $snail_numbers]
    for {set i 0} {$i < $n} {incr i} {
        for {set j 0} {$j < $n} {incr j} {
            if {$i == $j} continue
            set a [lindex $snail_numbers $i]
            set b [lindex $snail_numbers $j]
            set sum1 [magnitude [add [deep_copy $a] [deep_copy $b]]]
            set sum2 [magnitude [add [deep_copy $b] [deep_copy $a]]]
            if {$sum1 > $largest_magnitude} {set largest_magnitude $sum1}
            if {$sum2 > $largest_magnitude} {set largest_magnitude $sum2}
        }
    }

    puts $largest_magnitude
}

main