proc priority {item} {
    set ascii [scan $item %c]
    if {$ascii >= 97} {
        return [expr {$ascii - 96}]
    } else {
        return [expr {$ascii - 38}]
    }
}

set sum 0
set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    set len [string length $line]
    set first [string range $line 0 [expr {$len / 2 - 1}]]
    set second [string range $line [expr {$len / 2}] end]

    set seen [dict create]
    foreach item [split $first ""] {
        dict set seen $item 1
    }

    foreach item [split $second ""] {
        if {[dict exists $seen $item]} {
            set sum [expr {$sum + [priority $item]}]
            break
        }
    }
}
close $file
puts $sum