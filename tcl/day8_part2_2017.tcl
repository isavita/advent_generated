proc evaluateInstructions {filename} {
    array set registers {}
    set maxValue 0
    set highestEver 0

    set file [open $filename r]
    while {[gets $file line] >= 0} {
        set parts [split $line]
        set reg [lindex $parts 0]
        set op [lindex $parts 1]
        set value [lindex $parts 2]
        set condReg [lindex $parts 4]
        set condOp [lindex $parts 5]
        set condVal [lindex $parts 6]

        if {[info exists registers($reg)] == 0} {
            set registers($reg) 0
        }
        if {[info exists registers($condReg)] == 0} {
            set registers($condReg) 0
        }

        set conditionMet [expr {
            ($condOp eq ">" && $registers($condReg) > $condVal) ||
            ($condOp eq ">=" && $registers($condReg) >= $condVal) ||
            ($condOp eq "<" && $registers($condReg) < $condVal) ||
            ($condOp eq "<=" && $registers($condReg) <= $condVal) ||
            ($condOp eq "==" && $registers($condReg) == $condVal) ||
            ($condOp eq "!=" && $registers($condReg) != $condVal)
        }]

        if {$conditionMet} {
            set change [expr {$op eq "inc" ? $value : -$value}]
            set registers($reg) [expr {$registers($reg) + $change}]
            set highestEver [expr {max($highestEver, $registers($reg))}]
        }
    }
    close $file

    foreach val [array names registers] {
        set maxValue [expr {max($maxValue, $registers($val))}]
    }

    return [list $maxValue $highestEver]
}

set results [evaluateInstructions "input.txt"]
puts "Largest final register value: [lindex $results 0]"
puts "Highest value ever held: [lindex $results 1]"