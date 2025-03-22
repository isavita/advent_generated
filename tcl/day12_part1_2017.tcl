
proc find_group {program group pipes} {
    lappend group $program
    foreach p [dict get $pipes $program] {
        if {[lsearch -exact $group $p] == -1} {
            set group [find_group $p $group $pipes]
        }
    }
    return $group
}

proc main {} {
    set file [open "input.txt" r]
    set data [split [read $file] \n]
    close $file

    set pipes [dict create]
    foreach line $data {
        if {$line eq ""} continue
        set parts [split $line]
        set program [lindex $parts 0]
        set connected_to {}
        for {set i 2} {$i < [llength $parts]} {incr i} {
            set item [lindex $parts $i]
            regsub -all {,} $item "" item
            lappend connected_to $item
        }
        dict set pipes $program $connected_to
    }

    set group_0 [find_group 0 {} $pipes]
    puts [llength [lsort -unique $group_0]]
}

main
