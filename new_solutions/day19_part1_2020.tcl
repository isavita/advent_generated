proc readFile {filename} {
    set file [open $filename r]
    set lines [split [read $file] "\n"]
    close $file
    return $lines
}

proc parseRules {lines} {
    set rules [dict create]
    foreach line $lines {
        if {$line eq ""} break
        set parts [split $line ":"]
        if {[llength $parts] < 2} continue
        set ruleNum [string trim [lindex $parts 0]]
        set ruleDef [string trim [lindex $parts 1]]
        dict set rules $ruleNum $ruleDef
    }
    return $rules
}

proc buildRegex {rules ruleNum} {
    if {[dict exists $rules $ruleNum] == 0} {
        return ""
    }
    set rule [dict get $rules $ruleNum]
    if {[string first "\"" $rule] != -1} {
        return [string trim $rule "\""]
    }
    set regex "("
    set parts [split $rule "|"]
    foreach part $parts {
        set subrules [split $part]
        foreach subrule $subrules {
            append regex [buildRegex $rules $subrule]
        }
        append regex "|"
    }
    if {[string index $regex end] eq "|"} {
        set regex [string range $regex 0 end-1]
    }
    append regex ")"
    return $regex
}

proc countValidMessages {lines regex} {
    set count 0
    foreach line $lines {
        if {[regexp "^$regex$" $line]} {
            incr count
        }
    }
    return $count
}

set lines [readFile "input.txt"]
set rules [parseRules $lines]
set regex [buildRegex $rules 0]
set messages [lrange $lines [expr {[dict size $rules] + 1}] end]
set validCount [countValidMessages $messages $regex]
puts $validCount