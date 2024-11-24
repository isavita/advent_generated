
#!/usr/bin/env tclsh

# Read input file
set fp [open "input.txt" r]
set instructions [split [read $fp] "\n"]
close $fp

# Initialize registers
array set registers {a 1 b 0}

# Process instructions
set i 0
while {$i < [llength $instructions]} {
    set instruction [lindex $instructions $i]
    set parts [split $instruction]
    set op [lindex $parts 0]

    switch $op {
        "hlf" {
            set reg [lindex $parts 1]
            set registers($reg) [expr {$registers($reg) / 2}]
        }
        "tpl" {
            set reg [lindex $parts 1]
            set registers($reg) [expr {$registers($reg) * 3}]
        }
        "inc" {
            set reg [lindex $parts 1]
            incr registers($reg)
        }
        "jmp" {
            set offset [lindex $parts 1]
            incr i [expr {$offset - 1}]
        }
        "jie" {
            set reg [string index [lindex $parts 1] 0]
            if {$registers($reg) % 2 == 0} {
                set offset [lindex $parts 2]
                incr i [expr {$offset - 1}]
            }
        }
        "jio" {
            set reg [string index [lindex $parts 1] 0]
            if {$registers($reg) == 1} {
                set offset [lindex $parts 2]
                incr i [expr {$offset - 1}]
            }
        }
    }
    incr i
}

puts $registers(b)
