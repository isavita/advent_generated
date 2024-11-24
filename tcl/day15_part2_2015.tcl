
# Function to read and parse the input file
proc readIngredients {filename} {
    set file [open $filename r]
    set ingredients {}
    while {[gets $file line] >= 0} {
        if {[regexp {(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)} $line -> name capacity durability flavor texture calories]} {
            lappend ingredients [list $name $capacity $durability $flavor $texture $calories]
        }
    }
    close $file
    return $ingredients
}

# Function to calculate the score of a given combination of ingredients
proc calculateScore {ingredients amounts} {
    set capacity 0
    set durability 0
    set flavor 0
    set texture 0
    set calories 0

    foreach ingredient $ingredients amount $amounts {
        lassign $ingredient name cap dur fla tex cal
        incr capacity [expr {$cap * $amount}]
        incr durability [expr {$dur * $amount}]
        incr flavor [expr {$fla * $amount}]
        incr texture [expr {$tex * $amount}]
        incr calories [expr {$cal * $amount}]
    }

    # Ensure non-negative values
    set capacity [expr {$capacity < 0 ? 0 : $capacity}]
    set durability [expr {$durability < 0 ? 0 : $durability}]
    set flavor [expr {$flavor < 0 ? 0 : $flavor}]
    set texture [expr {$texture < 0 ? 0 : $texture}]

    # Calculate score
    set score [expr {$capacity * $durability * $flavor * $texture}]
    return [list $score $calories]
}

# Function to find the maximum score
proc findMaxScore {ingredients {calorieConstraint -1}} {
    set maxScore 0
    set numIngredients [llength $ingredients]
    set amounts [list]

    # Generate all combinations of amounts that sum to 100
    for {set i 0} {$i <= 100} {incr i} {
        for {set j 0} {$j <= 100 - $i} {incr j} {
            for {set k 0} {$k <= 100 - $i - $j} {incr k} {
                set l [expr {100 - $i - $j - $k}]
                set amounts [list $i $j $k $l]
                if {[llength $amounts] == $numIngredients} {
                    lassign [calculateScore $ingredients $amounts] score calories
                    if {($calorieConstraint == -1 || $calories == $calorieConstraint) && $score > $maxScore} {
                        set maxScore $score
                    }
                }
            }
        }
    }
    return $maxScore
}

# Main program
set ingredients [readIngredients "input.txt"]
puts "Part 1: [findMaxScore $ingredients]"
puts "Part 2: [findMaxScore $ingredients 500]"
