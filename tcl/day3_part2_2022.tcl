set sum 0
set groupLineCounter 0
set groupItems [list [dict create] [dict create] [dict create]]

proc itemPriority {item} {
    if {$item >= "a" && $item <= "z"} {
        return [expr {[scan $item %c] - [scan "a" %c] + 1}]
    }
    return [expr {[scan $item %c] - [scan "A" %c] + 27}]
}

set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    set itemsMap [dict create]
    foreach item [split $line ""] {
        dict incr itemsMap $item
    }
    lset groupItems $groupLineCounter $itemsMap
    incr groupLineCounter

    if {$groupLineCounter == 3} {
        set commonItems [dict create]
        foreach item [dict keys [lindex $groupItems 0]] {
            if {[dict exists [lindex $groupItems 1] $item] && [dict exists [lindex $groupItems 2] $item]} {
                dict incr commonItems $item
            }
        }
        foreach item [dict keys $commonItems] {
            set sum [expr {$sum + [itemPriority $item]}]
            break
        }
        set groupLineCounter 0
    }
}
close $file
puts $sum