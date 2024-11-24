
#!/usr/bin/env tclsh

set fp [open "input.txt" r]
set instructions [split [read $fp] "\n"]
close $fp

array set registers {a 0 b 0}

for {set i 0} {$i < [llength $instructions]} {incr i} {
    set inst [lindex $instructions $i]
    set parts [split $inst]
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
}

puts $registers(b)
