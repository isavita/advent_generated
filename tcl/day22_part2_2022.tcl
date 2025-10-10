
set f [open input.txt r]
set map {}
set width 0
while {[gets $f line] >= 0 && $line ne ""} {
    lappend map $line
    set width [expr {max($width, [string length $line])}]
}
gets $f path
close $f

for {set r 0} {$r < [llength $map]} {incr r} {
    set row [lindex $map $r]
    set len [string length $row]
    if {$len < $width} {
        set row [format "%-*s" $width $row]
        lset map $r $row
    }
}

set r 0
set c 0
set d 0
while {[string index [lindex $map $r] $c] ne "."} {incr c}

set dr {0 1 0 -1}
set dc {1 0 -1 0}

proc wrap {r c d} {
    if {$d == 3 && $r == 0 && $c >= 50 && $c < 100}   {return [list [expr {$c + 100}] 0 0]}
    if {$d == 2 && $c == 50 && $r >= 0 && $r < 50}    {return [list [expr {149 - $r}] 0 0]}
    if {$d == 3 && $r == 0 && $c >= 100 && $c < 150}  {return [list 199 [expr {$c - 100}] 3]}
    if {$d == 0 && $c == 149 && $r >= 0 && $r < 50}    {return [list [expr {149 - $r}] 99 2]}
    if {$d == 1 && $r == 49 && $c >= 100 && $c < 150}  {return [list [expr {$c - 50}] 99 2]}
    if {$d == 0 && $c == 99 && $r >= 50 && $r < 100}   {return [list 49 [expr {$r + 50}] 3]}
    if {$d == 2 && $c == 50 && $r >= 50 && $r < 100}  {return [list 100 [expr {$r - 50}] 1]}
    if {$d == 2 && $c == 0 && $r >= 100 && $r < 150}   {return [list [expr {149 - $r}] 50 0]}
    if {$d == 3 && $r == 100 && $c >= 0 && $c < 50}   {return [list [expr {$c + 50}] 50 0]}
    if {$d == 0 && $c == 99 && $r >= 100 && $r < 150}  {return [list [expr {149 - $r}] 149 2]}
    if {$d == 1 && $r == 149 && $c >= 50 && $c < 100}  {return [list [expr {$c + 100}] 49 2]}
    if {$d == 0 && $c == 49 && $r >= 150 && $r < 200} {return [list 149 [expr {$r - 100}] 3]}
    if {$d == 1 && $r == 199 && $c >= 0 && $c < 50}   {return [list 0 [expr {$c + 100}] 1]}
    if {$d == 2 && $c == 0 && $r >= 150 && $r < 200}  {return [list 0 [expr {$r - 100}] 1]}
    return [list $r $c $d]
}

set i 0
set len [string length $path]
while {$i < $len} {
    set ch [string index $path $i]
    if {[string is digit $ch]} {
        set j $i
        while {$j < $len && [string is digit [string index $path $j]]} {incr j}
        set steps [string range $path $i [expr {$j - 1}]]
        set i $j
        for {set k 0} {$k < $steps} {incr k} {
            set nr [expr {$r + [lindex $dr $d]}]
            set nc [expr {$c + [lindex $dc $d]}]
            set nd $d
            if {$nr < 0 || $nr >= [llength $map] || $nc < 0 || $nc >= $width || [string index [lindex $map $nr] $nc] eq " "} {
                lassign [wrap $r $c $d] nr nc nd
            }
            set cell [string index [lindex $map $nr] $nc]
            if {$cell eq "#"} break
            set r $nr
            set c $nc
            set d $nd
        }
    } else {
        if {$ch eq "R"} {
            set d [expr {($d + 1) % 4}]
        } elseif {$ch eq "L"} {
            set d [expr {($d + 3) % 4}]
        }
        incr i
    }
}

puts [expr {1000 * ($r + 1) + 4 * ($c + 1) + $d}]
