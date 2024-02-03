
use strict;
use warnings;

my $filename = "input.txt";
open(my $fh, '<', $filename) or die "Could not open file '$filename' $!";
my $line = <$fh>;
close($fh);

my @parts = split(' ', $line);
my $players = $parts[0];
my $lastMarble = $parts[6];

my @scores = (0) x $players;
my $current = { value => 0 };
$current->{next} = $current;
$current->{prev} = $current;

for (my $marble = 1; $marble <= $lastMarble; $marble++) {
    if ($marble % 23 == 0) {
        my $player = $marble % $players;
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
print "$maxScore\n";
