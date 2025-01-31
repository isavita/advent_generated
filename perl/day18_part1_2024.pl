
use strict;
use warnings;

# Define grid size
my $grid_size = 71;

# Read input from file
my @bytes;
open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @bytes, [split ',', $line];
}
close($fh);

# Function to calculate shortest path using BFS
sub shortest_path {
    my @grid = @{$_[0]};

    my @queue = ([0, 0, 0]); # x, y, steps
    my %visited;
    $visited{"0,0"} = 1;

    while (@queue) {
        my ($x, $y, $steps) = @{shift @queue};

        # Check if reached exit
        return $steps if ($x == $grid_size - 1 && $y == $grid_size - 1);

        # Explore neighbors
        for my $move ([0, 1], [0, -1], [1, 0], [-1, 0]) {
            my ($nx, $ny) = ($x + $move->[0], $y + $move->[1]);

            # Check boundaries and if cell is not corrupted
            if ($nx >= 0 && $nx < $grid_size && $ny >= 0 && $ny < $grid_size &&
                $grid[$ny][$nx] != 1 && !$visited{"$nx,$ny"}) {
                push @queue, [$nx, $ny, $steps + 1];
                $visited{"$nx,$ny"} = 1;
            }
        }
    }

    return -1; # No path found
}

# Simulate byte falls and find shortest path
my @grid = map { [(0) x $grid_size] } 0 .. $grid_size - 1;
for (my $i = 0; $i < 1024 && $i < @bytes; $i++) {
    my ($x, $y) = @{$bytes[$i]};
    $grid[$y][$x] = 1;
}

my $min_steps = shortest_path(\@grid);

print "Minimum steps needed: $min_steps\n";
