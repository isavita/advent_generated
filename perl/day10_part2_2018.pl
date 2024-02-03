
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @lines = <$fh>;
close($fh);

my $head = { next => undef };
my $tail = $head;
my $re = qr/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/;

foreach my $line (@lines) {
    my @split = $line =~ $re;
    next if scalar @split != 4;
    my $star = {
        x  => $split[0],
        y  => $split[1],
        vX => $split[2],
        vY => $split[3],
        next => undef
    };
    $tail->{next} = $star;
    $tail = $star;
}

my $smallestT = 0;
my $smallestArea = int(~0 >> 1);

for (my $t = 1; $t < 100000; $t++) {
    my ($maxX, $maxY, $minX, $minY) = (0, 0, 0, 0);

    for (my $temp = $head->{next}; $temp->{next}; $temp = $temp->{next}) {
        my $x = $temp->{x} + $temp->{vX} * $t;
        $maxX = $x if $maxX < $x;
        $minX = $x if $minX > $x;
        my $y = $temp->{y} + $temp->{vY} * $t;
        $maxY = $y if $maxY < $y;
        $minY = $y if $minY > $y;
    }

    my $lenX = $maxX - $minX + 1;
    my $lenY = $maxY - $minY + 1;
    my $area = $lenX + $lenY;

    if ($smallestArea > $area) {
        $smallestArea = $area;
        $smallestT = $t;
    }
}

print "$smallestT\n";

my $t = $smallestT;
my ($maxX, $maxY, $minX, $minY) = (0, 0, 0, 0);

for (my $temp = $head->{next}; $temp->{next}; $temp = $temp->{next}) {
    $temp->{x} += $temp->{vX} * $t;
    $maxX = $temp->{x} if $maxX < $temp->{x};
    $minX = $temp->{x} if $minX > $temp->{x};
    $temp->{y} += $temp->{vY} * $t;
    $maxY = $temp->{y} if $maxY < $temp->{y};
    $minY = $temp->{y} if $minY > $temp->{y};
}

my @mapper;
for (my $i = 0; $i < $maxY - $minY + 1; $i++) {
    $mapper[$i] = [(0) x ($maxX - $minX + 1)];
}

for (my $temp = $head->{next}; $temp->{next}; $temp = $temp->{next}) {
    $mapper[$temp->{y}][$temp->{x}] = 1;
}

for (my $i = 0; $i < scalar @mapper; $i++) {
    for (my $j = 0; $j < scalar @{$mapper[0]}; $j++) {
    }
}
