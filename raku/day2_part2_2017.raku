
sub MAIN {
    my @data = 'input.txt'.IO.lines.map(-> $line { $line.words.map(*.Int) });
    say @data.map(-> @row { @row.max - @row.min }).sum;
    my $sum2 = 0;
    for @data -> @row {
        for 0 ..^ @row.elems -> $i {
            for $i + 1 ..^ @row.elems -> $j {
                if @row[$i] % @row[$j] == 0 {
                    $sum2 += @row[$i] div @row[$j];
                } elsif @row[$j] % @row[$i] == 0 {
                    $sum2 += @row[$j] div @row[$i];
                }
            }
        }
    }
    say $sum2;
}
