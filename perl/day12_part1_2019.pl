
use strict;
use warnings;

sub Abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    } else {
        return $x;
    }
}

sub applyGravity {
    my $moons = shift;
    for (my $i = 0; $i < scalar(@$moons); $i++) {
        for (my $j = $i + 1; $j < scalar(@$moons); $j++) {
            if ($moons->[$i]{pos}{x} > $moons->[$j]{pos}{x}) {
                $moons->[$i]{vel}{x}--;
                $moons->[$j]{vel}{x}++;
            } elsif ($moons->[$i]{pos}{x} < $moons->[$j]{pos}{x}) {
                $moons->[$i]{vel}{x}++;
                $moons->[$j]{vel}{x}--;
            }

            if ($moons->[$i]{pos}{y} > $moons->[$j]{pos}{y}) {
                $moons->[$i]{vel}{y}--;
                $moons->[$j]{vel}{y}++;
            } elsif ($moons->[$i]{pos}{y} < $moons->[$j]{pos}{y}) {
                $moons->[$i]{vel}{y}++;
                $moons->[$j]{vel}{y}--;
            }

            if ($moons->[$i]{pos}{z} > $moons->[$j]{pos}{z}) {
                $moons->[$i]{vel}{z}--;
                $moons->[$j]{vel}{z}++;
            } elsif ($moons->[$i]{pos}{z} < $moons->[$j]{pos}{z}) {
                $moons->[$i]{vel}{z}++;
                $moons->[$j]{vel}{z}--;
            }
        }
    }
}

sub applyVelocity {
    my $moons = shift;
    foreach my $moon (@$moons) {
        $moon->{pos}{x} += $moon->{vel}{x};
        $moon->{pos}{y} += $moon->{vel}{y};
        $moon->{pos}{z} += $moon->{vel}{z};
    }
}

sub totalEnergy {
    my $moons = shift;
    my $total = 0;
    foreach my $m (@$moons) {
        my $pot = Abs($m->{pos}{x}) + Abs($m->{pos}{y}) + Abs($m->{pos}{z});
        my $kin = Abs($m->{vel}{x}) + Abs($m->{vel}{y}) + Abs($m->{vel}{z});
        $total += $pot * $kin;
    }
    return $total;
}

open my $fh, '<', 'input.txt' or die $!;
my @moons;
while (my $line = <$fh>) {
    my ($x, $y, $z) = $line =~ /<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/;
    push @moons, {pos => {x => $x, y => $y, z => $z}, vel => {x => 0, y => 0, z => 0}};
}

for (my $step = 0; $step < 1000; $step++) {
    applyGravity(\@moons);
    applyVelocity(\@moons);
}

print totalEnergy(\@moons) . "\n";
