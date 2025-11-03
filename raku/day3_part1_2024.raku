
sub MAIN {
    my $total = 0;
    for 'input.txt'.IO.slurp ~~ m:g/mul\((\d+)\,(\d+)\)/ -> $m {
        $total += $m[0] * $m[1]
    }
    say $total
}
