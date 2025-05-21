
sub MAIN {
    my @data = 'input.txt'.IO.lines.map: { .words.map: *.Int };

    my $possible-triangles = 0;

    for 0, 3 ... @data.end - 2 -> $i {
        for 0 .. 2 -> $j {
            my @sides = (@data[$i][$j], @data[$i+1][$j], @data[$i+2][$j]).sort;
            if @sides[0] + @sides[1] > @sides[2] {
                $possible-triangles++;
            }
        }
    }

    say $possible-triangles;
}
