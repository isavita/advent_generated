set file [open "input.txt" r]
set data [read $file]
close $file

set frequencyChanges [split $data "\n"]
set frequencies [dict create]
set currentFrequency 0
dict set frequencies $currentFrequency 1

while {1} {
    foreach change $frequencyChanges {
        set frequencyDelta [expr {$change + 0}]
        set currentFrequency [expr {$currentFrequency + $frequencyDelta}]
        if {[dict exists $frequencies $currentFrequency]} {
            puts $currentFrequency
            exit
        }
        dict set frequencies $currentFrequency 1
    }
}