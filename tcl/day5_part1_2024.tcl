
proc check_order {update rules} {
    foreach rule $rules {
        lassign $rule before after
        if {[lsearch $update $before] != -1 && [lsearch $update $after] != -1} {
            if {[lsearch $update $before] > [lsearch $update $after]} {
                return 0
            }
        }
    }
    return 1
}

proc get_middle_page {update} {
    set len [llength $update]
    if {$len % 2 == 0} {
        return ""
    } else {
        return [lindex $update [expr {$len / 2}]]
    }
}

set rules {}
set updates {}
set reading_rules 1
set total 0

if {[file exists "input.txt"]} {
    set file [open "input.txt" r]
    while {[gets $file line] != -1} {
        if {$line eq ""} {
            set reading_rules 0
            continue
        }
        if {$reading_rules} {
            lappend rules $line
        } else {
            lappend updates $line
        }
    }
    close $file
} else {
    puts "Error: input.txt not found"
    exit 1
}

foreach rule $rules {
    if {[regexp {(\d+)\|(\d+)} $rule match before after]} {
        lappend parsed_rules [list $before $after]
    }
}

foreach update $updates {
    set pages [split $update ","]
    if {[check_order $pages $parsed_rules]} {
        set middle_page [get_middle_page $pages]
        if {$middle_page ne ""} {
            incr total $middle_page
        }
    }
}

puts $total
