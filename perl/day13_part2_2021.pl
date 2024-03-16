use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my %points;
my @folds;
my $readingPoints = 1;

while (my $line = <$fh>) {
    chomp $line;
    if ($line eq "") {
        $readingPoints = 0;
        next;
    }
    if ($readingPoints) {
        my @parts = split(',', $line);
        my $x = $parts[0];
        my $y = $parts[1];
        $points{$x}{$y} = 1;
    } else {
        my @parts = split('=', $line);
        my $val = $parts[1];
        if ($parts[0] =~ /x/) {
            push @folds, [$val, 0];
        } else {
            push @folds, [0, $val];
        }
    }
}

close $fh;

foreach my $fold (@folds) {
    my %newPoints;
    foreach my $x (keys %points) {
        foreach my $y (keys %{$points{$x}}) {
            my $newX = $x;
            my $newY = $y;
            if ($fold->[0] != 0 && $x > $fold->[0]) {
                $newX = $fold->[0] - ($x - $fold->[0]);
            } elsif ($fold->[1] != 0 && $y > $fold->[1]) {
                $newY = $fold->[1] - ($y - $fold->[1]);
            }
            $newPoints{$newX}{$newY} = 1;
        }
    }
    %points = %newPoints;
    if ($folds[0] == $fold) {
        my $count = keys %points;
        print "Number of dots visible after first fold: $count\n";
    }
}

my $maxX = 0;
my $maxY = 0;

foreach my $x (keys %points) {
    $maxX = $x if $x > $maxX;
    foreach my $y (keys %{$points{$x}}) {
        $maxY = $y if $y > $maxY;
    }
}

my @grid;
for (my $i = 0; $i <= $maxY; $i++) {
    $grid[$i] = [];
    for (my $j = 0; $j <= $maxX; $j++) {
        $grid[$i][$j] = ' ';
    }
}

foreach my $x (keys %points) {
    foreach my $y (keys %{$points{$x}}) {
        $grid[$y][$x] = '#';
    }
}

foreach my $row (@grid) {
    print join("", @$row) . "\n";
}