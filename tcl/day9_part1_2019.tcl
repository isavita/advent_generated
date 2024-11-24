
#!/usr/bin/env tclsh

proc runIntcode {memoryVar} {
    upvar $memoryVar memory
    set ip 0
    set relativeBase 0
    set output 0

    while {1} {
        set opcode [expr {$memory($ip) % 100}]
        set modes [format "%03d" [expr {$memory($ip) / 100}]]

        proc getParam {ip modes relativeBaseVar memoryVar offset} {
            upvar $relativeBaseVar relativeBase
            upvar $memoryVar memory
            
            set mode [string index $modes end-[expr {$offset-1}]]
            if {$mode eq ""} {set mode 0}

            set param $memory([expr {$ip + $offset}])
            switch $mode {
                0 { return $memory($param) }
                1 { return $param }
                2 { return $memory([expr {$relativeBase + $param}]) }
            }
        }

        proc setParam {ip modes relativeBaseVar memoryVar offset value} {
            upvar $relativeBaseVar relativeBase
            upvar $memoryVar memory
            
            set mode [string index $modes end-[expr {$offset-1}]]
            if {$mode eq ""} {set mode 0}

            set param $memory([expr {$ip + $offset}])
            switch $mode {
                0 { set memory($param) $value }
                2 { set memory([expr {$relativeBase + $param}]) $value }
            }
        }

        switch $opcode {
            1 {
                setParam $ip $modes relativeBase memory 3 \
                    [expr {[getParam $ip $modes relativeBase memory 1] + \
                           [getParam $ip $modes relativeBase memory 2]}]
                incr ip 4
            }
            2 {
                setParam $ip $modes relativeBase memory 3 \
                    [expr {[getParam $ip $modes relativeBase memory 1] * \
                           [getParam $ip $modes relativeBase memory 2]}]
                incr ip 4
            }
            3 {
                setParam $ip $modes relativeBase memory 1 1
                incr ip 2
            }
            4 {
                set output [getParam $ip $modes relativeBase memory 1]
                incr ip 2
            }
            5 {
                if {[getParam $ip $modes relativeBase memory 1] != 0} {
                    set ip [getParam $ip $modes relativeBase memory 2]
                } else {
                    incr ip 3
                }
            }
            6 {
                if {[getParam $ip $modes relativeBase memory 1] == 0} {
                    set ip [getParam $ip $modes relativeBase memory 2]
                } else {
                    incr ip 3
                }
            }
            7 {
                if {[getParam $ip $modes relativeBase memory 1] < \
                    [getParam $ip $modes relativeBase memory 2]} {
                    setParam $ip $modes relativeBase memory 3 1
                } else {
                    setParam $ip $modes relativeBase memory 3 0
                }
                incr ip 4
            }
            8 {
                if {[getParam $ip $modes relativeBase memory 1] == \
                    [getParam $ip $modes relativeBase memory 2]} {
                    setParam $ip $modes relativeBase memory 3 1
                } else {
                    setParam $ip $modes relativeBase memory 3 0
                }
                incr ip 4
            }
            9 {
                incr relativeBase [getParam $ip $modes relativeBase memory 1]
                incr ip 2
            }
            99 {
                return $output
            }
            default {
                error "Unknown opcode: $opcode"
            }
        }
    }
}

# Read input from file
set file [open "input.txt" r]
set program [split [read -nonewline $file] ","]
close $file

# Convert input to array
array set memory {}
for {set i 0} {$i < [llength $program]} {incr i} {
    set memory($i) [lindex $program $i]
}

# Run Intcode and print output
puts [runIntcode memory]
