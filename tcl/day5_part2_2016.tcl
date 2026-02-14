package require md5

set f [open "input.txt" r]
set doorId [string trim [read $f]]
close $f

set password [list "" "" "" "" "" "" "" ""]
array set filled {}
set filledCount 0
set index 0

while {$filledCount < 8} {
    set hash [md5::md5 -hex "${doorId}${index}"]
    if {[string range $hash 0 4] eq "00000"} {
        set posChar [string index $hash 5]
        if {[string is digit -strict $posChar]} {
            set pos [expr {$posChar + 0}]
            if {$pos >= 0 && $pos < 8 && ![info exists filled($pos)]} {
                lset password $pos [string index $hash 6]
                set filled($pos) 1
                incr filledCount
            }
        }
    }
    incr index
}

puts [join $password ""]
