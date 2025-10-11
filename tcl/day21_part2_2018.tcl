
set r5 0
set seen [dict create]
set last 0
while 1 {
    set r3 [expr {$r5 | 65536}]
    set r5 7586220
    while 1 {
        set r1 [expr {$r3 & 255}]
        set r5 [expr {(($r5 + $r1) & 16777215) * 65899 & 16777215}]
        if {$r3 < 256} {
            if {[dict exists $seen $r5]} {puts $last; exit 0}
            dict set seen $r5 1
            set last $r5
            break
        }
        set r3 [expr {$r3 / 256}]
    }
}
