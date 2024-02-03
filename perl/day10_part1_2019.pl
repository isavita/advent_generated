
use strict;
use warnings;
use Math::Trig;

my @asteroids = readAsteroids("input.txt");
my $maxCount = findBestAsteroidLocation(\@asteroids);
print "$maxCount\n";

sub readAsteroids {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Cannot open file: $!";
    my @asteroids;
    while (my $line = <$fh>) {
        chomp $line;
        my @asteroidRow = map { $_ eq '#' ? 1 : 0 } split('', $line);
        push @asteroids, \@asteroidRow;
    }
    close $fh;
    return @asteroids;
}

sub findBestAsteroidLocation {
    my ($asteroids) = @_;
    my $maxCount = 0;
    for my $y (0 .. $#{$asteroids}) {
        for my $x (0 .. $#{$asteroids->[$y]}) {
            if ($asteroids->[$y][$x]) {
                my $count = countVisibleAsteroids($asteroids, $x, $y);
                if ($count > $maxCount) {
                    $maxCount = $count;
                }
            }
        }
    }
    return $maxCount;
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
