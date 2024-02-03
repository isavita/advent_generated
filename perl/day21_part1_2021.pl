
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my @lines = <$fh>;
close($fh);

my $player1Start = (split(':', $lines[0]))[1];
my $player2Start = (split(':', $lines[1]))[1];
my $player1Pos = $player1Start;
my $player2Pos = $player2Start;

my $player1Score = 0;
my $player2Score = 0;

my $dieRoll = 1;
my $rollCount = 0;

while (1) {
    # Player 1
    my $rolls = $dieRoll % 100 + ($dieRoll + 1) % 100 + ($dieRoll + 2) % 100;
    $rollCount += 3;
    $dieRoll += 3;

    $player1Pos = ($player1Pos + $rolls - 1) % 10 + 1;
    $player1Score += $player1Pos;

    if ($player1Score >= 1000) {
        print "Result: " . $player2Score * $rollCount . "\n";
        last;
    } else {
        # Player 2
        $rolls = $dieRoll % 100 + ($dieRoll + 1) % 100 + ($dieRoll + 2) % 100;
        $rollCount += 3;
        $dieRoll += 3;

        $player2Pos = ($player2Pos + $rolls - 1) % 10 + 1;
        $player2Score += $player2Pos;

        if ($player2Score >= 1000) {
            print $player1Score * $rollCount . "\n";
            last;
        }
    }
}
