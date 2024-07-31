set file [open "input.txt" r]
set totalCount 0
set groupAnswers {}

while {[gets $file line] >= 0} {
    if {$line eq ""} {
        set totalCount [expr {$totalCount + [llength $groupAnswers]}]
        set groupAnswers {}
    } else {
        foreach question [split $line ""] {
            if {[lsearch -exact $groupAnswers $question] == -1} {
                lappend groupAnswers $question
            }
        }
    }
}
set totalCount [expr {$totalCount + [llength $groupAnswers]}]
puts $totalCount
close $file