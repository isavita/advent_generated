
proc addr {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] + [lindex $registers [lindex $abc_values 1]]}]
    return $registers
}

proc addi {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] + [lindex $abc_values 1]}]
    return $registers
}

proc mulr {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] * [lindex $registers [lindex $abc_values 1]]}]
    return $registers
}

proc muli {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] * [lindex $abc_values 1]}]
    return $registers
}

proc banr {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] & [lindex $registers [lindex $abc_values 1]]}]
    return $registers
}

proc bani {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] & [lindex $abc_values 1]}]
    return $registers
}

proc borr {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] | [lindex $registers [lindex $abc_values 1]]}]
    return $registers
}

proc bori {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] | [lindex $abc_values 1]}]
    return $registers
}

proc setr {registers abc_values} {
    lset registers [lindex $abc_values 2] [lindex $registers [lindex $abc_values 0]]
    return $registers
}

proc seti {registers abc_values} {
    lset registers [lindex $abc_values 2] [lindex $abc_values 0]
    return $registers
}

proc gtir {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $abc_values 0] > [lindex $registers [lindex $abc_values 1]] ? 1 : 0}]
    return $registers
}

proc gtri {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] > [lindex $abc_values 1] ? 1 : 0}]
    return $registers
}

proc gtrr {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] > [lindex $registers [lindex $abc_values 1]] ? 1 : 0}]
    return $registers
}

proc eqir {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $abc_values 0] == [lindex $registers [lindex $abc_values 1]] ? 1 : 0}]
    return $registers
}

proc eqri {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] == [lindex $abc_values 1] ? 1 : 0}]
    return $registers
}

proc eqrr {registers abc_values} {
    lset registers [lindex $abc_values 2] [expr {[lindex $registers [lindex $abc_values 0]] == [lindex $registers [lindex $abc_values 1]] ? 1 : 0}]
    return $registers
}

proc solve {input_data} {
    set lines [split $input_data \n]
    set instruction_pointer [lindex [split [lindex $lines 0]] end]
    set instructions {}
    foreach line [lrange $lines 1 end] {
        if {[string trim $line] ne ""} {
            lappend instructions [split $line]
        }
    }

    set registers [list 0 0 0 0 0 0]
    while 1 {
        set inst_index [lindex $registers $instruction_pointer]
        if {$inst_index >= [llength $instructions]} {
            puts "Out of range instruction, terminating..."
            break
        }

        set inst [lindex $instructions $inst_index]
        set opcode [lindex $inst 0]
        set abc_values [lrange $inst 1 end]

        switch -- $opcode {
            addr  {set registers [addr $registers $abc_values]}
            addi  {set registers [addi $registers $abc_values]}
            mulr  {set registers [mulr $registers $abc_values]}
            muli  {set registers [muli $registers $abc_values]}
            banr  {set registers [banr $registers $abc_values]}
            bani  {set registers [bani $registers $abc_values]}
            borr  {set registers [borr $registers $abc_values]}
            bori  {set registers [bori $registers $abc_values]}
            setr  {set registers [setr $registers $abc_values]}
            seti  {set registers [seti $registers $abc_values]}
            gtir  {set registers [gtir $registers $abc_values]}
            gtri  {set registers [gtri $registers $abc_values]}
            gtrr  {set registers [gtrr $registers $abc_values]}
            eqir  {set registers [eqir $registers $abc_values]}
            eqri  {set registers [eqri $registers $abc_values]}
            eqrr  {set registers [eqrr $registers $abc_values]}
        }

        lset registers $instruction_pointer [expr {[lindex $registers $instruction_pointer] + 1}]

        if {[lindex $registers $instruction_pointer] == 28} {
            break
        }
        if {[lindex $registers $instruction_pointer] >= [llength $instructions]} {
            break
        }
    }
    return [lindex $registers 5]
}

proc main {} {
    set input_file "input.txt"
    set fp [open $input_file r]
    set input_data [read $fp]
    close $fp
    puts [solve $input_data]
}

main
