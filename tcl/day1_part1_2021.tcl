set count 0
set prev 0

set file [open "input.txt" r]
while {[gets $file line] >= 0} {
    set current [scan $line "%d"]
    if {$prev != 0 && $current > $prev} {
        incr count
    }
    set prev $current
}
close $file

puts $count