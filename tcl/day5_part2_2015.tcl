set file [open "input.txt" r]
set input [read $file]
close $file
set nice 0

proc passesRule1 {line} {
    set len [string length $line]
    for {set i 0} {$i < $len - 2} {incr i} {
        set toMatch [string range $line $i [expr {$i + 1}]]
        for {set j [expr {$i + 2}]} {$j < $len - 1} {incr j} {
            if {[string range $line $j [expr {$j + 1}]] eq $toMatch} {
                return 1
            }
        }
    }
    return 0
}

foreach line [split $input "\n"] {
    set rule1 [passesRule1 $line]
    set rule2 0
    set len [string length $line]
    for {set i 0} {$i < $len - 2} {incr i} {
        if {[string index $line $i] eq [string index $line [expr {$i + 2}]]} {
            set rule2 1
            break
        }
    }
    if {$rule1 && $rule2} {
        incr nice
    }
}

puts $nice