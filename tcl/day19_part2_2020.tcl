proc parseInput {filename} {
    set f [open $filename r]
    set text [string trim [read $f]]
    close $f

    lassign [split [string map {"\n\n" "\u0001"} $text] "\u0001"] rulesPart messagesPart
    set rules [dict create]

    foreach line [split $rulesPart "\n"] {
        if {[regexp {^(\d+): "([a-z])"$} $line -> num ch]} {
            dict set rules $num [dict create type char char $ch]
        } elseif {[regexp {^(\d+): (.+)$} $line -> num rhs]} {
            set options {}
            set sep "\u0001"
            foreach opt [split [string map [list " | " $sep] $rhs] $sep] {
                lappend options [split $opt " "]
            }
            dict set rules $num [dict create type seq options $options]
        }
    }

    set messages [split $messagesPart "\n"]
    return [list $rules $messages]
}

proc expandRule {rulesVar memoVar ruleNum} {
    upvar 1 $rulesVar rules
    upvar 1 $memoVar memo

    if {[dict exists $memo $ruleNum]} {
        return [dict get $memo $ruleNum]
    }

    set rule [dict get $rules $ruleNum]
    if {[dict get $rule type] eq "char"} {
        set resolved [list [dict get $rule char]]
        dict set memo $ruleNum $resolved
        return $resolved
    }

    set result {}
    foreach opt [dict get $rule options] {
        set acc [list ""]
        foreach sub $opt {
            set pieces [expandRule rules memo $sub]
            set next {}
            foreach a $acc {
                foreach p $pieces {
                    lappend next "${a}${p}"
                }
            }
            set acc $next
        }
        foreach s $acc {
            lappend result $s
        }
    }

    dict set memo $ruleNum $result
    return $result
}

proc chunkMessage {msg chunkLen} {
    set out {}
    for {set i 0} {$i < [string length $msg]} {incr i $chunkLen} {
        lappend out [string range $msg $i [expr {$i + $chunkLen - 1}]]
    }
    return $out
}

lassign [parseInput "input.txt"] rules messages
set memo [dict create]
set rule42 [expandRule rules memo 42]
set rule31 [expandRule rules memo 31]

set chunkLen [string length [lindex $rule42 0]]
set valid31ByChunk [dict create]
set valid42ByChunk [dict create]
foreach s $rule42 { dict set valid42ByChunk $s 1 }
foreach s $rule31 { dict set valid31ByChunk $s 1 }

set count 0
foreach msg $messages {
    if {[string length $msg] == 0 || [expr {[string length $msg] % $chunkLen}] != 0} {
        continue
    }

    set chunks [chunkMessage $msg $chunkLen]
    set n [llength $chunks]

    set prefix42 0
    while {$prefix42 < $n && [dict exists $valid42ByChunk [lindex $chunks $prefix42]]} {
        incr prefix42
    }

    set suffix31 0
    set idx [expr {$n - 1}]
    while {$idx >= 0 && [dict exists $valid31ByChunk [lindex $chunks $idx]]} {
        incr suffix31
        incr idx -1
    }

    # Valid for rule 0 in part 2: 42^(m+n) 31^n with n>=1 and m>=1.
    if {$suffix31 >= 1 && $prefix42 > $suffix31 && ($prefix42 + $suffix31) == $n} {
        incr count
    }
}

puts $count
