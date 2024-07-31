proc hashString {str} {
    set res 0
    foreach char [split $str ""] {
        set res [expr {$res + [scan $char %c]}]
        set res [expr {$res * 17 % 256}]
    }
    return $res
}

proc parseStep {stepStr} {
    set stepLabel [string trimright $stepStr "=-0123456789"]
    set numBox [hashString $stepLabel]
    set operation [string index $stepStr [string length $stepLabel]]
    if {$operation == "="} {
        set number [expr {[scan [string range $stepStr [string length $stepLabel] end] %d]}]
        return [list $stepLabel $numBox $operation $number]
    }
    return [list $stepLabel $numBox $operation 0]
}

proc solve {input} {
    set line [lindex $input 0]
    set steps [split $line ","]
    set res 0
    foreach step $steps {
        set res [expr {$res + [hashString $step]}]
    }
    return $res
}

proc readFile {fileName} {
    set file [open $fileName r]
    set content [read $file]
    close $file
    return [split [string trim $content] "\n"]
}

set input [readFile "input.txt"]
puts [solve $input]