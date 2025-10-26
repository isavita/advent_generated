
sub MAIN {
    my @e = 'input.txt'.IO.lines».Int;
    .say for
        (@e X @e).race.grep(-> ($a,$b) { $a+$b==2020 }).map(-> ($a,$b) { $a*$b }),
        (@e X @e X @e).race.grep(-> ($a,$b,$c) { $a+$b+$c==2020 }).map(-> ($a,$b,$c) { $a*$b*$c });
}
