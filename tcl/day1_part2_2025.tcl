
set fh [open input.txt r]
set input [split [read $fh] \n]
close $fh

set DIAL 100
set pos 50
set total 0

proc floorDiv {a b} {
    expr {int(floor(double($a)/$b))}
}

foreach raw $input {
    set line [string trim $raw]
    if {$line eq ""} continue

    set dir [string index $line 0]
    set amt [string range $line 1 end]

    if {$dir eq "R"} {
        set total [expr {$total + int(($pos + $amt)/$DIAL)}]
        set pos [expr {($pos + $amt)%$DIAL}]
    } elseif {$dir eq "L"} {
        set before [floorDiv [expr {$pos - 1}] $DIAL]
        set after  [floorDiv [expr {$pos - $amt - 1}] $DIAL]
        set total [expr {$total + $before - $after}]
        set pos [expr {($pos - $amt)%$DIAL}]
        if {$pos < 0} {incr pos $DIAL}
    } else {
        error "Unknown direction '$dir'"
    }
}

puts "The password is: $total"
