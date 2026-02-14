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
            set sep ""
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

lassign [parseInput "input.txt"] rules messages
set memo [dict create]
set rule42 [expandRule rules memo 42]
set rule31 [expandRule rules memo 31]

set re42 "(?:[join $rule42 |])"
set re31 "(?:[join $rule31 |])"

set count 0
foreach msg $messages {
    set ok 0
    for {set n 1} {$n < 10 && !$ok} {incr n} {
        set pattern "^(?:${re42})+(?:${re42}){${n}}(?:${re31}){${n}}$"
        if {[regexp $pattern $msg]} {
            set ok 1
        }
    }
    if {$ok} {
        incr count
    }
}

puts $count
