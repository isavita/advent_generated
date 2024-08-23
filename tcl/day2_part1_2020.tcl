set file [open "input.txt" r]
set validCount 0

while {[gets $file line] >= 0} {
    regexp {(\d+)-(\d+) (\w): (\w+)} $line -> min max letter password
    set count [string map [list $letter ""] $password]
    set count [expr {[string length $password] - [string length $count]}]
    if {$count >= $min && $count <= $max} {
        incr validCount
    }
}

close $file
puts $validCount