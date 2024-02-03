
use strict;
use warnings;

sub abs {
    my $x = shift;
    if ($x < 0) {
        return -$x;
    }
    return $x;
}

sub sign {
    my $x = shift;
    if ($x > 0) {
        return 1;
    } elsif ($x < 0) {
        return -1;
    }
    return 0;
}

open my $fh, '<', 'input.txt' or die "Failed to open input file: $!";
my @lines;
while (my $line = <$fh>) {
    chomp $line;
    my @parts = split / -> /, $line;
    my @start = split /,/, $parts[0];
    my @end = split /,/, $parts[1];

    my $x1 = $start[0];
    my $y1 = $start[1];
    my $x2 = $end[0];
    my $y2 = $end[1];

    push @lines, [$x1, $y1, $x2, $y2];
}
close $fh;

my %overlaps;

foreach my $line (@lines) {
    my ($x1, $y1, $x2, $y2) = @$line;

    my $xStep = sign($x2 - $x1);
    my $yStep = sign($y2 - $y1);
    my $steps = abs($x2 - $x1) + 1;
    if (abs($y2 - $y1) > abs($x2 - $x1)) {
        $steps = abs($y2 - $y1) + 1;
    }

    for (my $i = 0; $i < $steps; $i++) {
        my @point = ($x1 + $i * $xStep, $y1 + $i * $yStep);
        $overlaps{"@point"}++;
    }
}

my $count = 0;
foreach my $v (values %overlaps) {
    if ($v > 1) {
        $count++;
    }
}

print "$count\n";
