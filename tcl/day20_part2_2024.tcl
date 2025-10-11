
set f [open input.txt r]
set grid [split [read $f] \n]
close $f
set H [llength $grid]
set W [string length [lindex $grid 0]]
for {set r 0} {$r < $H} {incr r} {
    for {set c 0} {$c < $W} {incr c} {
        set ch [string index [lindex $grid $r] $c]
        if {$ch eq "S"} {set Sr $r; set Sc $c}
        if {$ch eq "E"} {set Er $r; set Ec $c}
        if {$ch eq "#"} {set wall($r,$c) 1} {set wall($r,$c) 0}
    }
}
proc bfs {srcR srcC distVar} {
    upvar 1 $distVar dist
    upvar 1 wall wall
    upvar 1 H H
    upvar 1 W W
    unset -nocomplain dist
    set q [list [list $srcR $srcC]]
    set dist($srcR,$srcC) 0
    set dirs {{1 0} {-1 0} {0 1} {0 -1}}
    while {[llength $q]} {
        set n [lindex $q 0]
        set q [lrange $q 1 end]
        lassign $n r c
        set d $dist($r,$c)
        foreach dir $dirs {
            lassign $dir dr dc
            set nr [expr {$r+$dr}]
            set nc [expr {$c+$dc}]
            if {$nr>=0&&$nr<$H&&$nc>=0&&$nc<$W&&!$wall($nr,$nc)&&![info exists dist($nr,$nc)]} {
                set dist($nr,$nc) [expr {$d+1}]
                lappend q [list $nr $nc]
            }
        }
    }
}
bfs $Sr $Sc distS
bfs $Er $Ec distE
set normal $distS($Er,$Ec)
if {![info exists normal]} {puts 0; exit}
set cnt 0
for {set sr 0} {$sr<$H} {incr sr} {
    for {set sc 0} {$sc<$W} {incr sc} {
        if {$wall($sr,$sc) || ![info exists distS($sr,$sc)]} continue
        set sd $distS($sr,$sc)
        for {set er 0} {$er<$H} {incr er} {
            for {set ec 0} {$ec<$W} {incr ec} {
                if {$wall($er,$ec) || ![info exists distE($er,$ec)]} continue
                set cheat [expr {abs($er-$sr)+abs($ec-$sc)}]
                if {$cheat<=20} {
                    set total [expr {$sd+$cheat+$distE($er,$ec)}]
                    if {$total<$normal && ($normal-$total)>=100} {incr cnt}
                }
            }
        }
    }
}
puts $cnt
