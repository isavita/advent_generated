set file [open "input.txt" r]
set offsets [split [read $file] "\n"]
close $file

set offsets [lmap offset $offsets {expr {$offset + 0}}]
set steps 0
set pos 0

while {$pos >= 0 && $pos < [llength $offsets]} {
    set jump [lindex $offsets $pos]
    if {$jump >= 3} {
        set offsets [lreplace $offsets $pos $pos [expr {$jump - 1}]]
    } else {
        set offsets [lreplace $offsets $pos $pos [expr {$jump + 1}]]
    }
    set pos [expr {$pos + $jump}]
    incr steps
}

puts $steps