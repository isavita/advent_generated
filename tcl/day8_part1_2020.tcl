set file [open "input.txt" r]
set instructions [split [read $file] "\n"]
close $file

proc executeBootCode {instructions} {
    set accumulator 0
    array set visited {}  ;# Initialize visited as an array
    set currentInstruction 0

    while {$currentInstruction < [llength $instructions]} {
        if {[info exists visited($currentInstruction)]} {
            return $accumulator
        }

        set visited($currentInstruction) 1
        set parts [split [lindex $instructions $currentInstruction] " "]
        set op [lindex $parts 0]
        set arg [lindex $parts 1]

        switch -- $op {
            "acc" {
                set accumulator [expr {$accumulator + $arg}]
                incr currentInstruction
            }
            "jmp" {
                set currentInstruction [expr {$currentInstruction + $arg}]
            }
            "nop" {
                incr currentInstruction
            }
        }
    }
    return $accumulator
}

puts [executeBootCode $instructions]