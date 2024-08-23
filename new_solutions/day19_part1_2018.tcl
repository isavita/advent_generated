proc executeProgram {fileName} {
    set registers [list 0 0 0 0 0 0]
    set instructions [list]
    set ipRegister 0

    set file [open $fileName r]
    while {[gets $file line] >= 0} {
        if {[string match "#ip *" $line]} {
            set ipRegister [lindex [split $line] 1]
        } else {
            lappend instructions [split $line]
        }
    }
    close $file

    set ip 0
    while {$ip >= 0 && $ip < [llength $instructions]} {
        set ins [lindex $instructions $ip]
        set opcode [lindex $ins 0]
        set a [lindex $ins 1]
        set b [lindex $ins 2]
        set c [lindex $ins 3]

        set registers [lreplace $registers $ipRegister $ipRegister $ip]

        switch -- $opcode {
            "addr" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] + [lindex $registers $b]}]] }
            "addi" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] + $b}]] }
            "mulr" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] * [lindex $registers $b]}]] }
            "muli" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] * $b}]] }
            "banr" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] & [lindex $registers $b]}]] }
            "bani" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] & $b}]] }
            "borr" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] | [lindex $registers $b]}]] }
            "bori" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] | $b}]] }
            "setr" { set registers [lreplace $registers $c $c [lindex $registers $a]] }
            "seti" { set registers [lreplace $registers $c $c $a] }
            "gtir" { set registers [lreplace $registers $c $c [expr {$a > [lindex $registers $b] ? 1 : 0}]] }
            "gtri" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] > $b ? 1 : 0}]] }
            "gtrr" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] > [lindex $registers $b] ? 1 : 0}]] }
            "eqir" { set registers [lreplace $registers $c $c [expr {$a == [lindex $registers $b] ? 1 : 0}]] }
            "eqri" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] == $b ? 1 : 0}]] }
            "eqrr" { set registers [lreplace $registers $c $c [expr {[lindex $registers $a] == [lindex $registers $b] ? 1 : 0}]] }
        }

        set ip [lindex $registers $ipRegister]
        incr ip
    }
    return [lindex $registers 0]
}

puts [executeProgram "input.txt"]