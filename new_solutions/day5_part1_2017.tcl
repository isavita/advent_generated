set file [open "input.txt" r]
set offsets [split [read $file] "\n"]
close $file

set offsets [lmap offset $offsets {expr {$offset + 0}}]
set steps 0
set position 0

while {$position >= 0 && $position < [llength $offsets]} {
    set jump [lindex $offsets $position]
    set offsets [lreplace $offsets $position $position [expr {$jump + 1}]]
    set position [expr {$position + $jump}]
    incr steps
}

puts $steps