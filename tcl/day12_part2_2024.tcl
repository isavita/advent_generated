
#!/usr/bin/env tclsh
set input [read [open input.txt r]]
set lines [split [string trim $input] \n]
set h [llength $lines]
set w [string length [lindex $lines 0]]

# padded grid: 1-cell border
set GRID [lrepeat [expr {$h+2}] [lrepeat [expr {$w+2}] .]]
array set VISITED {}
array set IN_REGION {}

# fill grid
for {set r 0} {$r < $h} {incr r} {
    set line [lindex $lines $r]
    for {set c 0} {$c < $w} {incr c} {
        lset GRID [expr {$r+1}] [expr {$c+1}] [string index $line $c]
    }
}

proc get {r c} {global GRID; lindex $GRID $r $c}
proc visited {r c} {global VISITED; expr {[info exists VISITED($r,$c)]}}
proc inRegion {r c} {global IN_REGION; expr {[info exists IN_REGION($r,$c)]}}

set P1 0
set P2 0

for {set sr 1} {$sr <= $h} {incr sr} {
    for {set sc 1} {$sc <= $w} {incr sc} {
        if {[visited $sr $sc]} continue
        set plant [get $sr $sc]
        set area 0
        set peri 0

        set q [list [list $sr $sc]]
        set VISITED($sr,$sc) 1
        set coords {}
        while {[llength $q]} {
            lassign [lindex $q 0] r c
            set q [lrange $q 1 end]
            incr area
            lappend coords $r $c
            foreach {dr dc} {-1 0 1 0 0 -1 0 1} {
                set nr [expr {$r+$dr}]
                set nc [expr {$c+$dc}]
                if {[get $nr $nc] ne $plant} {
                    incr peri
                } elseif {![visited $nr $nc]} {
                    set VISITED($nr,$nc) 1
                    lappend q [list $nr $nc]
                }
            }
        }
        incr P1 [expr {$area * $peri}]

        # Part 2 sides
        array unset IN_REGION
        foreach {r c} $coords {set IN_REGION($r,$c) 1}
        set topF 0; set botF 0; set lefF 0; set rigF 0
        set topA 0; set botA 0; set lefA 0; set rigA 0
        foreach {r c} $coords {
            if {[get [expr {$r-1}] $c] ne $plant} {incr topF}
            if {[get [expr {$r+1}] $c] ne $plant} {incr botF}
            if {[get $r [expr {$c-1}]] ne $plant} {incr lefF}
            if {[get $r [expr {$c+1}]] ne $plant} {incr rigF}

            if {[inRegion $r [expr {$c+1}]]} {
                if {[get [expr {$r-1}] $c] ne $plant && [get [expr {$r-1}] [expr {$c+1}]] ne $plant} {incr topA}
                if {[get [expr {$r+1}] $c] ne $plant && [get [expr {$r+1}] [expr {$c+1}]] ne $plant} {incr botA}
            }
            if {[inRegion [expr {$r+1}] $c]} {
                if {[get $r [expr {$c-1}]] ne $plant && [get [expr {$r+1}] [expr {$c-1}]] ne $plant} {incr lefA}
                if {[get $r [expr {$c+1}]] ne $plant && [get [expr {$r+1}] [expr {$c+1}]] ne $plant} {incr rigA}
            }
        }
        set sides [expr {($topF-$topA)+($botF-$botA)+($lefF-$lefA)+($rigF-$rigA)}]
        incr P2 [expr {$area * $sides}]
    }
}

puts $P1
puts $P2
