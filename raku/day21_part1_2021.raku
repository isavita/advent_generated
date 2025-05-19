
sub MAIN {
    my @lines = 'input.txt'.IO.lines;
    my $player1Pos = @lines[0].split(' ').tail.Int;
    my $player2Pos = @lines[1].split(' ').tail.Int;
    my $player1Score = 0;
    my $player2Score = 0;
    my $dieRoll = 1;
    my $rollCount = 0;

    loop {
        my $rolls = $dieRoll % 100 + ($dieRoll + 1) % 100 + ($dieRoll + 2) % 100;
        $rollCount += 3;
        $dieRoll += 3;
        $player1Pos = ($player1Pos - 1 + $rolls) % 10 + 1;
        $player1Score += $player1Pos;
        if $player1Score >= 1000 {
            say $player2Score * $rollCount;
            last;
        }

        $rolls = $dieRoll % 100 + ($dieRoll + 1) % 100 + ($dieRoll + 2) % 100;
        $rollCount += 3;
        $dieRoll += 3;
        $player2Pos = ($player2Pos - 1 + $rolls) % 10 + 1;
        $player2Score += $player2Pos;
        if $player2Score >= 1000 {
            say $player1Score * $rollCount;
            last;
        }
    }
}
