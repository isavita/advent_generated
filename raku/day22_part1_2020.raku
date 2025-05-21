
sub MAIN {
    my ($p1-str, $p2-str) = 'input.txt'.IO.slurp.split: "\n\n";

    my @p1 = $p1-str.split("\n")[1..*].map(*.Int);
    my @p2 = $p2-str.split("\n")[1..*].map(*.Int);

    while @p1.elems && @p2.elems {
        my $c1 = @p1.shift;
        my $c2 = @p2.shift;

        if $c1 > $c2 {
            @p1.push($c1, $c2);
        } else {
            @p2.push($c2, $c1);
        }
    }

    my @winner = @p1.elems ?? @p1 !! @p2;

    my $score = sum @winner.reverse.kv.map: { ($^k + 1) * $^v };
    say $score;
}
