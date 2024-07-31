set data [read [open "input.txt"]]
regexp {row (\d+), column (\d+)} $data match row column

set row [expr {$row}]
set column [expr {$column}]
set pos [expr {($row + $column - 2) * ($row + $column - 1) / 2 + $column}]
set startCode 20151125
set multiplier 252533
set modulus 33554393

set code $startCode
for {set i 1} {$i < $pos} {incr i} {
    set code [expr {($code * $multiplier) % $modulus}]
}

puts $code