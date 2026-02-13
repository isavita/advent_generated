
#!/usr/bin/tclsh

# Function to calculate how many minutes to wait until enough resources are collected to build a robot.
# Returns the number of full minutes of collection required.
proc time_to_build {current robots cost} {
    if {$current >= $cost} { return 0 }
    if {$robots == 0} { return 100 } ;# Large value to effectively discard this path
    return [expr {($cost - $current + $robots - 1) / $robots}]
}

# The core solver uses a jump-based Depth-First Search (DFS) with pruning to find the maximum geodes.
# It skips minute-by-minute steps by jumping directly to the time when a robot can be built.
proc solve {bp time_limit} {
    # Extract robot costs from the blueprint list
    lassign $bp id ore_c clay_c obs_c_ore obs_c_clay geo_c_ore geo_c_obs

    # A robot type shouldn't be built if we already produce enough of that resource to build any robot every minute.
    set max_ore [expr {max($ore_c, $clay_c, $obs_c_ore, $geo_c_ore)}]
    set max_clay $obs_c_clay
    set max_obsidian $geo_c_obs
    
    set max_geodes 0
    # Stack stores states: {time_left, ore_robots, clay_robots, obsidian_robots, geode_robots, ore, clay, obsidian, geodes}
    set stack [list [list $time_limit 1 0 0 0 0 0 0 0]]

    while {[llength $stack] > 0} {
        set state [lindex $stack end]
        set stack [lreplace $stack end end]
        lassign $state t r1 r2 r3 r4 m1 m2 m3 m4

        # Calculate the total geodes that will exist at the end if no more robots are built
        set current_total [expr {$m4 + $r4 * $t}]
        if {$current_total > $max_geodes} { set max_geodes $current_total }
        
        # Stop exploration if no time remains or if it's mathematically impossible to beat the current max
        if {$t <= 1} continue
        set theoretical_max [expr {$m4 + $r4 * $t + ($t * ($t - 1)) / 2}]
        if {$theoretical_max <= $max_geodes} continue

        # Resource capping optimization: excess resources that can't be spent are capped to reduce state space.
        # We cap resources to (time_left * max_requirement).
        if {$m1 > $t * $max_ore} { set m1 [expr {$t * $max_ore}] }
        if {$m2 > $t * $max_clay} { set m2 [expr {$t * $max_clay}] }
        if {$m3 > $t * $max_obsidian} { set m3 [expr {$t * $max_obsidian}] }

        # Generate branches for each robot type. We push them to the stack in order of least-to-most promising 
        # (Ore, Clay, Obsidian, Geode) so that Geode robots are processed first (Last-In-First-Out).
        
        # 1. Build Ore Robot
        if {$r1 < $max_ore} {
            set dt [time_to_build $m1 $r1 $ore_c]
            if {$t - $dt - 1 > 0} {
                set nt [expr {$t - $dt - 1}]
                lappend stack [list $nt [expr {$r1 + 1}] $r2 $r3 $r4 \
                    [expr {$m1 + $r1 * ($dt + 1) - $ore_c}] \
                    [expr {$m2 + $r2 * ($dt + 1)}] \
                    [expr {$m3 + $r3 * ($dt + 1)}] \
                    [expr {$m4 + $r4 * ($dt + 1)}]]
            }
        }

        # 2. Build Clay Robot
        if {$r2 < $max_clay} {
            set dt [time_to_build $m1 $r1 $clay_c]
            if {$t - $dt - 1 > 0} {
                set nt [expr {$t - $dt - 1}]
                lappend stack [list $nt $r1 [expr {$r2 + 1}] $r3 $r4 \
                    [expr {$m1 + $r1 * ($dt + 1) - $clay_c}] \
                    [expr {$m2 + $r2 * ($dt + 1)}] \
                    [expr {$m3 + $r3 * ($dt + 1)}] \
                    [expr {$m4 + $r4 * ($dt + 1)}]]
            }
        }

        # 3. Build Obsidian Robot
        if {$r3 < $max_obsidian} {
            set dt [expr {max([time_to_build $m1 $r1 $obs_c_ore], [time_to_build $m2 $r2 $obs_c_clay])}]
            if {$t - $dt - 1 > 0} {
                set nt [expr {$t - $dt - 1}]
                lappend stack [list $nt $r1 $r2 [expr {$r3 + 1}] $r4 \
                    [expr {$m1 + $r1 * ($dt + 1) - $obs_c_ore}] \
                    [expr {$m2 + $r2 * ($dt + 1) - $obs_c_clay}] \
                    [expr {$m3 + $r3 * ($dt + 1)}] \
                    [expr {$m4 + $r4 * ($dt + 1)}]]
            }
        }

        # 4. Build Geode Robot
        set dt [expr {max([time_to_build $m1 $r1 $geo_c_ore], [time_to_build $m3 $r3 $geo_c_obs])}]
        if {$t - $dt - 1 > 0} {
            set nt [expr {$t - $dt - 1}]
            lappend stack [list $nt $r1 $r2 $r3 [expr {$r4 + 1}] \
                [expr {$m1 + $r1 * ($dt + 1) - $geo_c_ore}] \
                [expr {$m2 + $r2 * ($dt + 1)}] \
                [expr {$m3 + $r3 * ($dt + 1) - $geo_c_obs}] \
                [expr {$m4 + $r4 * ($dt + 1)}]]
            # Pruning: If a Geode robot can be built immediately, prioritising it is almost always optimal.
            if {$dt == 0} continue
        }
    }
    return $max_geodes
}

# Load blueprints from input.txt
set blueprints {}
if {[file exists "input.txt"]} {
    set fp [open "input.txt" r]
    while {[gets $fp line] >= 0} {
        set nums [regexp -all -inline {\d+} $line]
        if {[llength $nums] > 0} { lappend blueprints $nums }
    }
    close $fp
}

# Part 1: Calculate the sum of quality levels for all blueprints at 24 minutes.
set part1 0
foreach bp $blueprints {
    set geodes [solve $bp 24]
    set part1 [expr {$part1 + [lindex $bp 0] * $geodes}]
}
puts "$part1"

# Part 2: Calculate the product of maximum geodes for the first three blueprints at 32 minutes.
set part2 1
set count 0
foreach bp $blueprints {
    if {$count >= 3} break
    set geodes [solve $bp 32]
    set part2 [expr {$part2 * $geodes}]
    incr count
}
puts "$part2"

