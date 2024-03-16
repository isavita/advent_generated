use strict;
use warnings;
use List::Util qw( max );
use Math::Trig;

sub readAsteroids {
    my $filename = shift;
    open my $fh, '<', $filename or die $!;
    my @asteroids;
    while (my $line = <$fh>) {
        chomp $line;
        my @asteroidRow = map { $_ eq '#' ? 1 : 0 } split //, $line;
        push @asteroids, \@asteroidRow;
    }
    close $fh;
    return \@asteroids;
}

sub vaporizeAsteroids {
    my ($asteroids, $station) = @_;
    my @targets;
    for my $y (0 .. $#{$asteroids}) {
        for my $x (0 .. $#{$asteroids->[$y]}) {
            if ($asteroids->[$y][$x] && !($x == $station->[0] && $y == $station->[1])) {
                my $angle = atan2($y - $station->[1], $x - $station->[0]);
                my $dist = sqrt(($x - $station->[0])**2 + ($y - $station->[1])**2);
                $angle += 2 * pi if $angle < -pi/2;
                push @targets, { x => $x, y => $y, angle => $angle, dist => $dist };
            }
        }
    }

    @targets = sort { $a->{angle} <=> $b->{angle} || $a->{dist} <=> $b->{dist} } @targets;

    my @vaporized;
    my $lastAngle = -9999999;
    while (@targets) {
        my @uniqueAngles;
        for my $target (@targets) {
            if ($target->{angle} != $lastAngle) {
                push @uniqueAngles, $target;
                $lastAngle = $target->{angle};
            }
        }
        push @vaporized, @uniqueAngles;
        @targets = grep { $_->{angle} != $lastAngle } @targets;
    }

    return \@vaporized;
}

sub findBestAsteroidLocation {
    my $asteroids = shift;
    my ($bestLocation, $maxCount);
    for my $y (0 .. $#{$asteroids}) {
        for my $x (0 .. $#{$asteroids->[$y]}) {
            if ($asteroids->[$y][$x]) {
                my $count = countVisibleAsteroids($asteroids, $x, $y);
                if ($count > $maxCount) {
                    $maxCount = $count;
                    $bestLocation = [$x, $y];
                }
            }
        }
    }
    return ($bestLocation, $maxCount);
}

sub countVisibleAsteroids {
    my ($asteroids, $x, $y) = @_;
    my %angles;
    for my $otherY (0 .. $#{$asteroids}) {
        for my $otherX (0 .. $#{$asteroids->[$otherY]}) {
            if ($asteroids->[$otherY][$otherX] && !($otherX == $x && $otherY == $y)) {
                my $angle = atan2($otherY - $y, $otherX - $x);
                $angles{$angle} = 1;
            }
        }
    }
    return scalar keys %angles;
}

my $asteroids = readAsteroids("input.txt");
my ($station, $dummy) = findBestAsteroidLocation($asteroids);
my $vaporized = vaporizeAsteroids($asteroids, $station);
if (@$vaporized >= 200) {
    my $result = $vaporized->[199]{x} * 100 + $vaporized->[199]{y};
    print "$result\n";
} else {
    print "Less than 200 asteroids were vaporized.\n";
}