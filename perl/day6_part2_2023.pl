
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Error opening file: $!";
my $time = 0;
my $distance = 0;

while (my $line = <$fh>) {
    chomp $line;
    next if $line eq "";
    my @parts = split(':', $line);
    my $value = join('', split(' ', $parts[1])); # Remove spaces
    if ($time == 0) {
        $time = int($value);
    } else {
        $distance = int($value);
    }
}

close($fh);

sub calculateWaysToWinLongRace {
    my ($time, $record) = @_;
    my $waysToWin = 0;
    for (my $holdTime = 1; $holdTime < $time; $holdTime++) {
        my $travelTime = $time - $holdTime;
        my $distance = $holdTime * $travelTime;
        if ($distance > $record) {
            $waysToWin++;
        }
    }
    return $waysToWin;
}

my $waysToWin = calculateWaysToWinLongRace($time, $distance);
print "$waysToWin\n";
