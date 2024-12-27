
proc allChar {s ch} {
    foreach c [split $s ""] {
        if {$c ne $ch} {return 0}
    }
    return 1
}
proc parseLock {b} {
    set h {}
    for {set c 0} {$c < 5} {incr c} {
        set cnt 0
        for {set r 1} {$r < 7} {incr r} {
            if {[string index [lindex $b $r] $c] eq "#"} {
                incr cnt
            } else {
                break
            }
        }
        lappend h $cnt
    }
    return $h
}
proc parseKey {b} {
    set h {}
    for {set c 0} {$c < 5} {incr c} {
        set cnt 0
        for {set r 5} {$r >= 0} {incr r -1} {
            if {[string index [lindex $b $r] $c] eq "#"} {
                incr cnt
            } else {
                break
            }
        }
        lappend h $cnt
    }
    return $h
}
proc fits {lock key} {
    for {set i 0} {$i < 5} {incr i} {
        if {[lindex $lock $i] + [lindex $key $i] > 5} {
            return 0
        }
    }
    return 1
}
if {[catch {open "input.txt" r} f]} {
    puts "0"
    return
}
set raw {}
while {[gets $f line] >= 0} {
    set line [string trim $line]
    if {$line ne ""} {
        lappend raw $line
    }
}
close $f
if {[llength $raw] % 7 != 0} {
    puts "0"
    return
}
set locks {}
set keys {}
for {set i 0} {$i + 7 <= [llength $raw]} {incr i 7} {
    set block [lrange $raw $i [expr {$i + 6}]]
    set valid 1
    foreach ln $block {
        if {[string length $ln] < 5} {
            set valid 0
            break
        }
    }
    if {!$valid} {continue}
    if {[allChar [lindex $block 0] "#"]} {
        lappend locks [parseLock $block]
    } else {
        lappend keys [parseKey $block]
    }
}
set count 0
foreach lock $locks {
    foreach key $keys {
        if {[fits $lock $key]} {
            incr count
        }
    }
}
puts $count
