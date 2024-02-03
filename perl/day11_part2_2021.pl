
use strict;
use warnings;

my @grid = readInput("input.txt");
my $step = 0;

while (1) {
    $step++;
    my $flashes = simulateStep(\@grid);
    if ($flashes == 100) {
        last;
    }
}

print "$step\n";

sub readInput {
    my ($filename) = @_;
    open my $fh, '<', $filename or die $!;
    my @grid;

    while (my $line = <$fh>) {
        chomp $line;
        my @row = split('', $line);
        my @nums = map { int($_) } @row;
        push @grid, \@nums;
    }

    close $fh;
    return @grid;
}

sub simulateStep {
    my ($grid) = @_;
    my $flashes = 0;
    my %flashed;

    foreach my $y (0 .. $#{$grid}) {
        foreach my $x (0 .. $#{$grid->[$y]}) {
            $grid->[$y][$x]++;
        }
    }

    foreach my $y (0 .. $#{$grid}) {
        foreach my $x (0 .. $#{$grid->[$y]}) {
            if ($grid->[$y][$x] > 9) {
                $flashes += flash($grid, $x, $y, \%flashed);
            }
        }
    }

    foreach my $coords (keys %flashed) {
        my ($x, $y) = split(',', $coords);
        $grid->[$y][$x] = 0;
    }

    return $flashes;
}

sub flash {
    my ($grid, $x, $y, $flashed) = @_;
    return 0 if $flashed->{"$x,$y"};

    $flashed->{"$x,$y"} = 1;
    my $flashes = 1;
    my @directions = ([-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]);

    foreach my $dir (@directions) {
        my ($newX, $newY) = ($x + $dir->[0], $y + $dir->[1]);
        if ($newX >= 0 && $newX < @{$grid->[0]} && $newY >= 0 && $newY < @{$grid}) {
            $grid->[$newY][$newX]++;
            if ($grid->[$newY][$newX] > 9) {
                $flashes += flash($grid, $newX, $newY, $flashed);
            }
        }
    }

    return $flashes;
}
