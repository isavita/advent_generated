
use strict;
use warnings;

my @grid = read_input("input.txt");
my $totalFlashes = 0;

for (my $step = 0; $step < 100; $step++) {
    $totalFlashes += simulate_step(\@grid);
}

print "$totalFlashes\n";

sub read_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die $!;
    my @grid;

    while (my $line = <$fh>) {
        chomp $line;
        my @row = split('', $line);
        for (my $i = 0; $i < scalar @row; $i++) {
            $row[$i] = int($row[$i]);
        }
        push @grid, \@row;
    }

    close $fh;
    return @grid;
}

sub simulate_step {
    my ($grid) = @_;
    my $flashes = 0;
    my %flashed;

    foreach my $y (0 .. $#{$grid}) {
        foreach my $x (0 .. $#{$grid->[$y]}) {
            $grid->[$y][$x]++;
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
        if ($newX >= 0 && $newX < scalar @{$grid->[0]} && $newY >= 0 && $newY < scalar @{$grid}) {
            $grid->[$newY][$newX]++;
            if ($grid->[$newY][$newX] > 9) {
                $flashes += flash($grid, $newX, $newY, $flashed);
            }
        }
    }

    return $flashes;
}
