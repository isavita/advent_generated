
sub main {
    my @positions = 'input.txt'.IO.slurp.split(',').map(*.Int).sort;
    my $median = @positions[ (@positions.elems / 2).floor ];
    my $total-fuel = @positions.map({ ($_ - $median).abs }).sum;
    print $total-fuel;
}

main;
