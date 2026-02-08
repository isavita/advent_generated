
set f [open input.txt r]
set data [read $f]
close $f

set workflows [dict create]
set parts [list]

# parse
set lines [split $data \n]
set wf_phase 1
foreach line $lines {
    set line [string trim $line]
    if {$line eq ""} {
        set wf_phase 0
        continue
    }
    if {$wf_phase} {
        # workflow line
        set brace [string first \{ $line]
        set name [string range $line 0 [expr {$brace-1}]]
        set rules_str [string range $line [expr {$brace+1}] end-1]
        set rules [list]
        foreach rule [split $rules_str ,] {
            if {[string first : $rule] != -1} {
                # conditional rule
                set cat [string index $rule 0]
                set op  [string index $rule 1]
                set rest [string range $rule 2 end]
                set colon [string first : $rest]
                set val [string range $rest 0 [expr {$colon-1}]]
                set dest [string range $rest [expr {$colon+1}] end]
                lappend rules [list $cat $op $val $dest]
            } else {
                # default rule
                lappend rules [list "" "" "" $rule]
            }
        }
        dict set workflows $name $rules
    } else {
        # part line
        set x 0; set m 0; set a 0; set s 0
        foreach token [split [string trim $line \{\}] ,] {
            lassign [split $token =] k v
            set $k $v
        }
        lappend parts [list $x $m $a $s]
    }
}

proc eval_part {part} {
    global workflows
    set current "in"
    while {1} {
        set rules [dict get $workflows $current]
        foreach rule $rules {
            lassign $rule cat op val dest
            if {$cat eq ""} {
                set match 1
            } else {
                set v [lindex $part [lsearch {x m a s} $cat]]
                set match [expr {$op eq "<" ? $v < $val : $v > $val}]
            }
            if {$match} {
                if {$dest eq "A"} {return 1}
                if {$dest eq "R"} {return 0}
                set current $dest
                break
            }
        }
    }
}

set total 0
foreach part $parts {
    if {[eval_part $part]} {
        lassign $part x m a s
        incr total [expr {$x + $m + $a + $s}]
    }
}
puts $total
