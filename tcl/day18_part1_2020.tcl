
proc evaluate {expression} {
    while {[string first "(" $expression] != -1} {
        set start [string last "(" $expression]
        set tail [string range $expression $start end]
        set end [expr {$start + [string first ")" $tail]}]
        set sub_expr [string range $expression [expr {$start + 1}] [expr {$end - 1}]]
        set sub_result [evaluate $sub_expr]
        set expression [string range $expression 0 [expr {$start - 1}]][string range $sub_result 0 end][string range $expression [expr {$end + 1}] end]
    }

    set tokens [split $expression]
    set total [lindex $tokens 0]
    set index 1

    while {$index < [llength $tokens]} {
        set op [lindex $tokens $index]
        set val [lindex $tokens [expr {$index + 1}]]

        if {$op eq "+"} {
            set total [expr {$total + $val}]
        } elseif {$op eq "*"} {
            set total [expr {$total * $val}]
        }
        incr index 2
    }
    return $total
}

set inputFile "input.txt"
set fileId [open $inputFile r]
set lines [split [read $fileId] "\n"]
close $fileId

set result 0
foreach line $lines {
    if {$line ne ""} {
    set result [expr {$result + [evaluate $line]}]
    }
}

puts $result
