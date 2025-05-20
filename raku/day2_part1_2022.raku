
sub MAIN {
    my %score-map = (
        "A X" => 4, "A Y" => 8, "A Z" => 3,
        "B X" => 1, "B Y" => 5, "B Z" => 9,
        "C X" => 7, "C Y" => 2, "C Z" => 6
    );

    my $total-score = 0;

    for "input.txt".IO.lines -> $line {
        $total-score += %score-map{$line};
    }

    say $total-score;
}
