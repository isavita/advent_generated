
use strict;
use warnings;

my @reindeers = readReindeerDetails("input.txt");
simulateRaceWithPoints(\@reindeers, 2503);
my $maxPoints = findMaxPoints(\@reindeers);
print "$maxPoints\n";

sub readReindeerDetails {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Error reading input: $!";
    
    my @reindeers;
    while (my $line = <$fh>) {
        my @parts = split(' ', $line);
        my $speed = $parts[3];
        my $flyTime = $parts[6];
        my $restTime = $parts[13];

        push @reindeers, {speed => $speed, flyTime => $flyTime, restTime => $restTime, flying => 1, distance => 0, points => 0, timeInMode => 0};
    }

    close($fh);
    return @reindeers;
}

sub simulateRaceWithPoints {
    my ($reindeers, $totalSeconds) = @_;
    for my $i (0..$totalSeconds-1) {
        my $maxDistance = 0;
        for my $j (0..$#{$reindeers}) {
            my $reindeer = $reindeers->[$j];
            if ($reindeer->{flying}) {
                $reindeer->{distance} += $reindeer->{speed};
            }
            $reindeer->{timeInMode}++;
            if (($reindeer->{flying} && $reindeer->{timeInMode} == $reindeer->{flyTime}) || (!$reindeer->{flying} && $reindeer->{timeInMode} == $reindeer->{restTime})) {
                $reindeer->{flying} = !$reindeer->{flying};
                $reindeer->{timeInMode} = 0;
            }
            $maxDistance = $reindeer->{distance} if $reindeer->{distance} > $maxDistance;
        }
        for my $j (0..$#{$reindeers}) {
            $reindeers->[$j]{points}++ if $reindeers->[$j]{distance} == $maxDistance;
        }
    }
}

sub findMaxPoints {
    my ($reindeers) = @_;
    my $maxPoints = 0;
    for my $reindeer (@$reindeers) {
        $maxPoints = $reindeer->{points} if $reindeer->{points} > $maxPoints;
    }
    return $maxPoints;
}
