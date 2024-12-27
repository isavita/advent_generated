
#!/usr/bin/tclsh

proc is_valid_triangle {a b c} {
    expr {($a + $b > $c) && ($a + $c > $b) && ($b + $c > $a)}
}

set valid_count 0
if {[catch {open "input.txt" r} file_handle]} {
    puts "Error opening input.txt: $file_handle"
    exit 1
}

while {[gets $file_handle line] != -1} {
    if {[regexp {^\s*(\d+)\s+(\d+)\s+(\d+)\s*$} $line -> a b c]} {
        if {[is_valid_triangle $a $b $c]} {
            incr valid_count
        }
    } else {
        puts "Warning: Invalid input line: $line"
    }
}

close $file_handle
puts $valid_count
