set totalSum 0
set file [open "input.txt" r]
set regex {Game (\d+): (.+)}
set cubeRegex {(\d+) (red|green|blue)}

while {[gets $file line] >= 0} {
    if {[regexp $regex $line match gameId rounds]} {
        set isValid 1
        set roundsList [split $rounds ";"]
        
        foreach round $roundsList {
            set cubes [regexp -all -inline $cubeRegex $round]
            set red 0
            set green 0
            set blue 0
            
            foreach cube $cubes {
                set count [lindex $cube 0]
                set color [lindex $cube 1]
                switch -- $color {
                    red {incr red $count}
                    green {incr green $count}
                    blue {incr blue $count}
                }
                if {$red > 12 || $green > 13 || $blue > 14} {
                    set isValid 0
                    break
                }
            }
            if {!$isValid} {break}
        }
        if {$isValid} {
            incr totalSum $gameId
        }
    }
}
close $file
puts $totalSum