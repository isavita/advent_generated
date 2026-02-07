set f [open "input.txt"]
set rows {}
while {[gets $f line] >= 0} {
    if {$line eq ""} continue
    lappend rows $line
}
close $f
set rowsCount [llength $rows]
set cols [string length [lindex $rows 0]]
set acc 0
for {set y 0} {$y < $rowsCount} {incr y} {
    set row [lindex $rows $y]
    for {set x 0} {$x < $cols} {incr x} {
        if {[string index $row $x] ne "@"} continue
        set cnt 0
        for {set dy -1} {$dy <= 1} {incr dy} {
            for {set dx -1} {$dx <= 1} {incr dx} {
                if {$dx == 0 && $dy == 0} continue
                set ny [expr {$y + $dy}]
                set nx [expr {$x + $dx}]
                if {$nx >=0 && $nx < $cols && $ny >=0 && $ny < $rowsCount} {
                    if {[string index [lindex $rows $ny] $nx] eq "@"} {incr cnt}
                }
            }
        }
        if {$cnt < 4} {incr acc}
    }
}
puts $acc