package require md5

set f [open "input.txt" r]
set doorId [string trim [read $f]]
close $f

set password [list "" "" "" "" "" "" "" ""]
set filledCount 0
set index 0

while {$filledCount < 8} {
    set hash [string tolower [md5::md5 -hex "${doorId}${index}"]]
    if {[string match 00000* $hash]} {
        set posChar [string index $hash 5]
        # "0" is 48, "7" is 55 — a single scan avoids string is + expr + range check
        set code [scan $posChar %c]
        if {$code >= 48 && $code <= 55} {
            set pos [expr {$code - 48}]
            if {[lindex $password $pos] eq ""} {
                lset password $pos [string index $hash 6]
                incr filledCount
            }
        }
    }
    incr index
}
puts [join $password ""]
