
sub MAIN {
    my %counts;

    for 'input.txt'.IO.lines -> $line {
        my ($coords1, $coords2) = $line.trim.split(' -> ');
        my ($x1, $y1) = $coords1.split(',').map(*.Int);
        my ($x2, $y2) = $coords2.split(',').map(*.Int);

        if $x1 == $x2 {
            for ($y1 min $y2) .. ($y1 max $y2) -> $y {
                %counts{$x1 => $y}++;
            }
        }
        elsif $y1 == $y2 {
            for ($x1 min $x2) .. ($x1 max $x2) -> $x {
                %counts{$x => $y1}++;
            }
        }
    }

    say %counts.values.grep(* >= 2).elems;
}
