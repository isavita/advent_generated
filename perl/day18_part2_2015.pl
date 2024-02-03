
use strict;
use warnings;

my $gridSize = 100;
my $steps = 100;

sub countOnNeighbors {
    my ($grid, $x, $y) = @_;
    my $on = 0;
    for my $dx (-1..1) {
        for my $dy (-1..1) {
            next if $dx == 0 && $dy == 0;
            my $nx = $x + $dx;
            my $ny = $y + $dy;
            if ($nx >= 0 && $nx < $gridSize && $ny >= 0 && $ny < $gridSize && $grid->[$nx][$ny]) {
                $on++;
            }
        }
    }
    return $on;
}

sub step {
    my ($grid) = @_;
    my @newGrid;
    for my $i (0..$gridSize-1) {
        $newGrid[$i] = [(0) x $gridSize];
    }

    for my $x (0..$gridSize-1) {
        for my $y (0..$gridSize-1) {
            my $onNeighbors = countOnNeighbors($grid, $x, $y);
            if ($grid->[$x][$y]) {
                $newGrid[$x][$y] = $onNeighbors == 2 || $onNeighbors == 3;
            } else {
                $newGrid[$x][$y] = $onNeighbors == 3;
            }
        }
    }

    $newGrid[0][0] = 1;
    $newGrid[0][$gridSize-1] = 1;
    $newGrid[$gridSize-1][0] = 1;
    $newGrid[$gridSize-1][$gridSize-1] = 1;

    return \@newGrid;
}

open my $fh, '<', 'input.txt' or die "Error opening file: $!";
my @grid;
my $y = 0;
while (my $line = <$fh>) {
    chomp $line;
    my @row = map { $_ eq '#' ? 1 : 0 } split //, $line;
    $grid[$y++] = \@row;
}

$grid[0][0] = 1;
$grid[0][$gridSize-1] = 1;
$grid[$gridSize-1][0] = 1;
$grid[$gridSize-1][$gridSize-1] = 1;

for (my $i = 0; $i < $steps; $i++) {
    @grid = @{step(\@grid)};
}

my $onCount = 0;
for my $row (@grid) {
    $onCount += grep { $_ } @$row;
}

print "$onCount\n";
