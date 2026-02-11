
set fp [open "input.txt" r]
set content [read $fp]
close $fp

set modules [dict create]
foreach line [split [string trim $content] "\n"] {
    if {$line eq ""} continue
    regexp {^([%&]?)([^ ]+) -> (.+)$} $line -> prefix name destPart
    set dests {}
    foreach d [split $destPart ","] {
        lappend dests [string trim $d]
    }
    dict set modules $name [dict create prefix $prefix dests $dests state 0 memory {}]
}

dict for {name data} $modules {
    foreach d [dict get $data dests] {
        if {[dict exists $modules $d] && [dict get $modules $d prefix] eq "&"} {
            dict set modules $d memory $name 0
        }
    }
}

set low 0
set high 0

for {set i 0} {$i < 1000} {incr i} {
    set q [list [list "button" "broadcaster" 0]]
    while {[llength $q] > 0} {
        set q [lassign $q curr]
        lassign $curr from to val

        if {$val == 0} { incr low } else { incr high }
        if {![dict exists $modules $to]} continue

        set prefix [dict get $modules $to prefix]
        set send -1

        if {$prefix eq "%"} {
            if {$val == 0} {
                set state [expr {![dict get $modules $to state]}]
                dict set modules $to state $state
                set send $state
            }
        } elseif {$prefix eq "&"} {
            dict set modules $to memory $from $val
            set send 0
            dict for {src v} [dict get $modules $to memory] {
                if {$v == 0} {
                    set send 1
                    break
                }
            }
        } else {
            set send $val
        }

        if {$send != -1} {
            foreach d [dict get $modules $to dests] {
                lappend q [list $to $d $send]
            }
        }
    }
}

puts [expr {wide($low) * $high}]
