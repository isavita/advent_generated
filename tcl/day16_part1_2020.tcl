proc isValidForAnyRule {value rules} {
    foreach rule $rules {
        set ranges [lrange $rule 1 end]
        foreach rng $ranges {
            if {$value >= [lindex $rng 0] && $value <= [lindex $rng 1]} {
                return 1
            }
        }
    }
    return 0
}

set file [open "input.txt" r]
set rules {}
set errorRate 0
set scanningRules 1

while {[gets $file line] >= 0} {
    if {$line eq ""} {continue}
    if {[string match "your ticket:*" $line] || [string match "nearby tickets:*" $line]} {
        set scanningRules 0
        continue
    }
    if {$scanningRules} {
        regexp {^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$} $line -> name r1 r2 r3 r4
        lappend rules [list $name [list [expr {$r1}] [expr {$r2}]] [list [expr {$r3}] [expr {$r4}]]]
    } else {
        foreach value [split $line ","] {
            set val [expr {$value + 0}]
            if {! [isValidForAnyRule $val $rules]} {
                set errorRate [expr {$errorRate + $val}]
            }
        }
    }
}

close $file
puts $errorRate