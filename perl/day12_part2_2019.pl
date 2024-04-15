use strict;
use warnings;
use Math::BigInt;

sub applyGravity {
    my ($moons, $axis) = @_;
    for my $i (0 .. $#$moons) {
        for my $j ($i + 1 .. $#$moons) {
            if ($axis eq 'x') {
                if ($moons->[$i]{pos}{x} > $moons->[$j]{pos}{x}) {
                    $moons->[$i]{vel}{x}--;
                    $moons->[$j]{vel}{x}++;
                } elsif ($moons->[$i]{pos}{x} < $moons->[$j]{pos}{x}) {
                    $moons->[$i]{vel}{x}++;
                    $moons->[$j]{vel}{x}--;
                }
            } elsif ($axis eq 'y') {
                if ($moons->[$i]{pos}{y} > $moons->[$j]{pos}{y}) {
                    $moons->[$i]{vel}{y}--;
                    $moons->[$j]{vel}{y}++;
                } elsif ($moons->[$i]{pos}{y} < $moons->[$j]{pos}{y}) {
                    $moons->[$i]{vel}{y}++;
                    $moons->[$j]{vel}{y}--;
                }
            } elsif ($axis eq 'z') {
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
}

sub applyVelocity {
    my ($moons, $axis) = @_;
    for my $moon (@$moons) {
        if ($axis eq 'x') {
            $moon->{pos}{x} += $moon->{vel}{x};
        } elsif ($axis eq 'y') {
            $moon->{pos}{y} += $moon->{vel}{y};
        } elsif ($axis eq 'z') {
            $moon->{pos}{z} += $moon->{vel}{z};
        }
    }
}

sub findCycle {
    my ($moons, $initialMoons, $axis) = @_;
    my $steps = 1;
    while (1) {
        applyGravity($moons, $axis);
        applyVelocity($moons, $axis);
        my $match = 1;
        for my $i (0 .. $#$moons) {
            if ($axis eq 'x') {
                if ($moons->[$i]{pos}{x} != $initialMoons->[$i]{pos}{x} || $moons->[$i]{vel}{x} != $initialMoons->[$i]{vel}{x}) {
                    $match = 0;
                }
            } elsif ($axis eq 'y') {
                if ($moons->[$i]{pos}{y} != $initialMoons->[$i]{pos}{y} || $moons->[$i]{vel}{y} != $initialMoons->[$i]{vel}{y}) {
                    $match = 0;
                }
            } elsif ($axis eq 'z') {
                if ($moons->[$i]{pos}{z} != $initialMoons->[$i]{pos}{z} || $moons->[$i]{vel}{z} != $initialMoons->[$i]{vel}{z}) {
                    $match = 0;
                }
            }
        }
        return $steps if $match;
        $steps++;
    }
}

sub lcm {
    my ($a, $b) = @_;
    my $bigA = Math::BigInt->new($a);
    my $bigB = Math::BigInt->new($b);
    return $bigA * $bigB / $bigA->bgcd($bigB);
}

my @moons;
my @initialMoons;
open my $file, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
while (my $line = <$file>) {
    chomp $line;
    my ($x, $y, $z) = $line =~ /<x=(-?\d+), y=(-?\d+), z=(-?\d+)>/;
    push @moons, {pos => {x => $x, y => $y, z => $z}, vel => {x => 0, y => 0, z => 0}};
    push @initialMoons, {pos => {x => $x, y => $y, z => $z}, vel => {x => 0, y => 0, z => 0}};
}
close $file;

my $cycleX = findCycle(\@moons, \@initialMoons, 'x');
my $cycleY = findCycle(\@moons, \@initialMoons, 'y');
my $cycleZ = findCycle(\@moons, \@initialMoons, 'z');

my $lcmXY = lcm($cycleX, $cycleY);
my $lcmXYZ = lcm($lcmXY, $cycleZ);

print "$lcmXYZ\n";