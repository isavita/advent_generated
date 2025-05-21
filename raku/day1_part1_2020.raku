
sub MAIN {
    my @expenses = "input.txt".IO.lines.map: *.Int;
    my ($x, $y) = @expenses.combinations(2).first: { .sum == 2020 };
    say $x * $y;
}
