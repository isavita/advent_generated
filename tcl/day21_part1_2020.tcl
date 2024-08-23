set file [open "input.txt" r]
set lines [split [read $file] "\n"]
close $file

array set ingredientCount {}
array set allergenMap {}
array set ingredientAllergenMap {}

proc intersect {list1 list2} {
    set result {}
    foreach item $list1 {
        if {[lsearch -exact $list2 $item] != -1} {
            lappend result $item
        }
    }
    return $result
}

foreach line $lines {
    regexp {(.*) \(contains (.*)\)} $line -> ingredientsStr allergensStr
    set ingredients [split $ingredientsStr " "]
    set allergens [split $allergensStr ", "]

    foreach ingredient $ingredients {
        incr ingredientCount($ingredient)
    }

    foreach allergen $allergens {
        if {[info exists allergenMap($allergen)]} {
            set allergenMap($allergen) [intersect $allergenMap($allergen) $ingredients]
        } else {
            set allergenMap($allergen) $ingredients
        }
    }
}

foreach {allergen ingredients} [array get allergenMap] {
    foreach ingredient $ingredients {
        set ingredientAllergenMap($ingredient) 1
    }
}

set count 0
foreach {ingredient cnt} [array get ingredientCount] {
    if {![info exists ingredientAllergenMap($ingredient)]} {
        set count [expr {$count + $cnt}]
    }
}

puts $count