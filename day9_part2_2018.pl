
use strict;
use warnings;

my @players;
my $lastMarble;

open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split(' ', $line);
    @players = ($parts[0]);
    $lastMarble = $parts[6];
}
close($fh);

$lastMarble *= 100;
print playMarbleGame(\@players, $lastMarble);

sub playMarbleGame {
    my ($players, $lastMarble) = @_;
    my @scores = (0) x $players->[0];
    my $current = { value => 0 };
    $current->{next} = $current;
    $current->{prev} = $current;

    for (my $marble = 1; $marble <= $lastMarble; $marble++) {
        if ($marble % 23 == 0) {
            my $player = $marble % $players->[0];
            for (my $i = 0; $i < 7; $i++) {
                $current = $current->{prev};
            }
            $scores[$player] += $marble + $current->{value};
            $current->{prev}{next} = $current->{next};
            $current->{next}{prev} = $current->{prev};
            $current = $current->{next};
        } else {
            $current = $current->{next};
            my $newMarble = { value => $marble, prev => $current, next => $current->{next} };
            $current->{next}{prev} = $newMarble;
            $current->{next} = $newMarble;
            $current = $newMarble;
        }
    }

    my $maxScore = 0;
    foreach my $score (@scores) {
        if ($score > $maxScore) {
            $maxScore = $score;
        }
    }
    return $maxScore;
}
