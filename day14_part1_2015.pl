
use strict;
use warnings;

my @reindeers = readReindeerDetails("input.txt");
simulateRace(\@reindeers, 2503);
my $maxDistance = findMaxDistance(@reindeers);
print "$maxDistance\n";

sub readReindeerDetails {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Error reading input: $!";
    my @reindeers;
    while (my $line = <$fh>) {
        my @parts = split(' ', $line);
        my $speed = $parts[3];
        my $flyTime = $parts[6];
        my $restTime = $parts[13];
        push @reindeers, { speed => $speed, flyTime => $flyTime, restTime => $restTime, flying => 1, distance => 0, timeInMode => 0 };
    }
    close $fh;
    return @reindeers;
}

sub simulateRace {
    my ($reindeers, $totalSeconds) = @_;
    for my $i (0..$totalSeconds-1) {
        foreach my $reindeer (@$reindeers) {
            if ($reindeer->{flying}) {
                $reindeer->{distance} += $reindeer->{speed};
                $reindeer->{timeInMode}++;
                if ($reindeer->{timeInMode} == $reindeer->{flyTime}) {
                    $reindeer->{flying} = 0;
                    $reindeer->{timeInMode} = 0;
                }
            } else {
                $reindeer->{timeInMode}++;
                if ($reindeer->{timeInMode} == $reindeer->{restTime}) {
                    $reindeer->{flying} = 1;
                    $reindeer->{timeInMode} = 0;
                }
            }
        }
    }
}

sub findMaxDistance {
    my @reindeers = @_;
    my $maxDistance = 0;
    foreach my $reindeer (@reindeers) {
        if ($reindeer->{distance} > $maxDistance) {
            $maxDistance = $reindeer->{distance};
        }
    }
    return $maxDistance;
}
