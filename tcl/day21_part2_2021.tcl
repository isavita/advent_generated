
# Global cache for memoization
global cache

# Recursive procedure to calculate wins from a given state
proc play {p1_pos p2_pos s1 s2 rollsLeft isP1Turn cacheName} {
    upvar 1 $cacheName cache

    # Create unique key for the current state
    set key [join [list $p1_pos $p2_pos $s1 $s2 $rollsLeft $isP1Turn] ,]

    # Check cache
    if {[info exists cache($key)]} {
        return $cache($key)
    }

    # Handle state after 3 rolls (rollsLeft == 0)
    if {$rollsLeft == 0} {
        # Calculate scores after adding the position
        set next_s1 $s1
        set next_s2 $s2

        if {$isP1Turn} {
            set next_s1 [expr {$s1 + $p1_pos}]
        } else {
            set next_s2 [expr {$s2 + $p2_pos}]
        }

        # Check for win after scoring
        if {$next_s1 >= 21} {
             set result [list 1 0]
             set cache($key) $result
             return $result
        }
        if {$next_s2 >= 21} {
             set result [list 0 1]
             set cache($key) $result
             return $result
        }

        # No win yet, transition to the next player's turn
        # The wins from this state are the wins from the game starting next turn
        set result [play $p1_pos $p2_pos $next_s1 $next_s2 3 [expr {!$isP1Turn}] $cacheName]
        set cache($key) $result
        return $result
    }

    # Handle state during a player's turn (rollsLeft > 0)
    set totalWins1 0
    set totalWins2 0

    # Iterate over possible dice rolls (1, 2, 3)
    for {set roll 1} {$roll <= 3} {incr roll} {
        # Calculate the next position for the current player
        set next_p1_pos $p1_pos
        set next_p2_pos $p2_pos

        if {$isP1Turn} {
            set next_p1_pos [expr {$p1_pos + $roll}]
             if {$next_p1_pos > 10} { set next_p1_pos [expr {$next_p1_pos - 10}] }
        } else {
            set next_p2_pos [expr {$p2_pos + $roll}]
             if {$next_p2_pos > 10} { set next_p2_pos [expr {$next_p2_pos - 10}] }
        }

        # Recurse for the state after this roll
        # Scores do not change until rollsLeft becomes 0
        set res [play $next_p1_pos $next_p2_pos $s1 $s2 [expr {$rollsLeft - 1}] $isP1Turn $cacheName]
        set totalWins1 [expr {$totalWins1 + [lindex $res 0]}]
        set totalWins2 [expr {$totalWins2 + [lindex $res 1]}]
    }

    # Store and return the combined results for this state
    set result [list $totalWins1 $totalWins2]
    set cache($key) $result
    return $result
}

# Procedure to parse starting positions from input.txt
proc parseInput {filename} {
    set f [open $filename r]
    set content [read $f]
    close $f

    set lines [split [string trim $content] \n]

    set parts1 [split [lindex $lines 0] " "]
    set pos1 [lindex $parts1 end]

    set parts2 [split [lindex $lines 1] " "]
    set pos2 [lindex $parts2 end]

    return [list $pos1 $pos2]
}

# Main execution procedure
proc main {} {
    global cache
    array set cache {}

    set positions [parseInput "input.txt"]
    set p1_start_pos [lindex $positions 0]
    set p2_start_pos [lindex $positions 1]

    set initial_s1 0
    set initial_s2 0
    set initial_rollsLeft 3
    set initial_isP1Turn 1

    set wins [play $p1_start_pos $p2_start_pos $initial_s1 $initial_s2 $initial_rollsLeft $initial_isP1Turn cache]

    set wins1 [lindex $wins 0]
    set wins2 [lindex $wins 1]

    if {$wins1 > $wins2} {
        puts $wins1
    } else {
        puts $wins2
    }
}

main
