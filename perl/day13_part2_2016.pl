
use strict;
use warnings;

my $favoriteNumber = 1362;
my %visited;
my @queue;
my $steps = 0;

sub isWall {
    my ($x, $y) = @_;
    my $num = $x*$x + 3*$x + 2*$x*$y + $y + $y*$y + $favoriteNumber;
    my $bits = 0;
    while ($num > 0) {
        if ($num % 2 == 1) {
            $bits++;
        }
        $num /= 2;
    }
    return $bits % 2 != 0;
}

sub bfsMaxSteps {
    my ($start, $maxSteps) = @_;
    my ($x, $y) = @$start;
    $visited{"$x,$y"} = 1;
    push @queue, $start;

    while (@queue && $steps < $maxSteps) {
        my $size = scalar @queue;
        for (my $i = 0; $i < $size; $i++) {
            my $point = shift @queue;

            foreach my $delta ([1, 0], [-1, 0], [0, 1], [0, -1]) {
                my ($dx, $dy) = @$delta;
                my $next = [$point->[0] + $dx, $point->[1] + $dy];
                my ($nx, $ny) = @$next;
                if ($nx >= 0 && $ny >= 0 && !isWall($nx, $ny) && !$visited{"$nx,$ny"}) {
                    $visited{"$nx,$ny"} = 1;
                    push @queue, $next;
                }
            }
        }
        $steps++;
    }

    return scalar keys %visited;
}

open my $fh, '<', 'input.txt' or die $!;
my @lines = <$fh>;
close $fh;

my $start = [1, 1];
my $reachableLocations = bfsMaxSteps($start, 50);
print "$reachableLocations\n";
