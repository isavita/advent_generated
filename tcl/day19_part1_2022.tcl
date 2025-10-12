proc parse_blueprint {line} {
    set numbers [regexp -all -inline {\d+} $line]
    return [list \
        id [lindex $numbers 0] \
        ore_robot [lindex $numbers 1] \
        clay_robot [lindex $numbers 2] \
        obsidian_robot [list [lindex $numbers 3] [lindex $numbers 4]] \
        geode_robot [list [lindex $numbers 5] [lindex $numbers 6]]]
}

proc simulate {blueprint time} {
    array set bp $blueprint
    set max_ore_cost [tcl::mathfunc::max $bp(ore_robot) $bp(clay_robot) [lindex $bp(obsidian_robot) 0] [lindex $bp(geode_robot) 0]]
    
    set cache [dict create]
    set max_geodes 0
    
    proc dfs {time ore clay obsidian ore_robots clay_robots obsidian_robots geode} {
        upvar bp bp max_ore_cost max_ore_cost cache cache max_geodes max_geodes
        
        set key "$time $ore $clay $obsidian $ore_robots $clay_robots $obsidian_robots $geode"
        if {[dict exists $cache $key]} {
            return [dict get $cache $key]
        }
        
        if {$time == 0} {
            dict set cache $key $geode
            return $geode
        }
        
        if {$geode + ($time * ($time - 1)) / 2 <= $max_geodes} {
            dict set cache $key 0
            return 0
        }
        
        set max_geode $geode
        
        if {$ore >= [lindex $bp(geode_robot) 0] && $obsidian >= [lindex $bp(geode_robot) 1]} {
            set result [dfs [expr {$time - 1}] \
                [expr {$ore + $ore_robots - [lindex $bp(geode_robot) 0]}] \
                [expr {$clay + $clay_robots}] \
                [expr {$obsidian + $obsidian_robots - [lindex $bp(geode_robot) 1]}] \
                $ore_robots $clay_robots $obsidian_robots \
                [expr {$geode + $time - 1}]]
            dict set cache $key $result
            return $result
        }
        
        if {$ore >= [lindex $bp(obsidian_robot) 0] && $clay >= [lindex $bp(obsidian_robot) 1] && $obsidian_robots < [lindex $bp(geode_robot) 1]} {
            set result [dfs [expr {$time - 1}] \
                [expr {$ore + $ore_robots - [lindex $bp(obsidian_robot) 0]}] \
                [expr {$clay + $clay_robots - [lindex $bp(obsidian_robot) 1]}] \
                [expr {$obsidian + $obsidian_robots}] \
                $ore_robots $clay_robots [expr {$obsidian_robots + 1}] $geode]
            set max_geode [tcl::mathfunc::max $max_geode $result]
        }
        
        if {$ore >= $bp(clay_robot) && $clay_robots < [lindex $bp(obsidian_robot) 1]} {
            set result [dfs [expr {$time - 1}] \
                [expr {$ore + $ore_robots - $bp(clay_robot)}] \
                [expr {$clay + $clay_robots}] \
                [expr {$obsidian + $obsidian_robots}] \
                $ore_robots [expr {$clay_robots + 1}] $obsidian_robots $geode]
            set max_geode [tcl::mathfunc::max $max_geode $result]
        }
        
        if {$ore >= $bp(ore_robot) && $ore_robots < $max_ore_cost} {
            set result [dfs [expr {$time - 1}] \
                [expr {$ore + $ore_robots - $bp(ore_robot)}] \
                [expr {$clay + $clay_robots}] \
                [expr {$obsidian + $obsidian_robots}] \
                [expr {$ore_robots + 1}] $clay_robots $obsidian_robots $geode]
            set max_geode [tcl::mathfunc::max $max_geode $result]
        }
        
        set result [dfs [expr {$time - 1}] \
            [expr {min($ore + $ore_robots, $time * $max_ore_cost - $ore_robots * ($time - 1))}] \
            [expr {$clay + $clay_robots}] \
            [expr {$obsidian + $obsidian_robots}] \
            $ore_robots $clay_robots $obsidian_robots $geode]
        set max_geode [tcl::mathfunc::max $max_geode $result]
        
        set max_geodes [tcl::mathfunc::max $max_geodes $max_geode]
        dict set cache $key $max_geode
        return $max_geode
    }
    
    return [dfs $time 0 0 0 1 0 0 0]
}

proc solve {blueprints {time 24}} {
    set total_quality 0
    foreach blueprint $blueprints {
        set geodes [simulate $blueprint $time]
        array set bp $blueprint
        set quality [expr {$bp(id) * $geodes}]
        incr total_quality $quality
    }
    return $total_quality
}

proc main {} {
    set file [open "input.txt" r]
    set blueprints {}
    while {[gets $file line] != -1} {
        lappend blueprints [parse_blueprint $line]
    }
    close $file
    
    set result [solve $blueprints]
    puts "The sum of the quality levels of all blueprints is: $result"
}

main