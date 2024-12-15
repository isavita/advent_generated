
#!/usr/bin/perl
use strict;
use warnings;

# Read the input file into an array of lines
my @grid;
open(my $fh, '<', 'input.txt') or die "Could not open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @grid, [ split //, $line ];
}
close $fh;

my $rows = scalar @grid;
my $cols = scalar @{$grid[0]};

# Function to simulate beam movement
sub simulate_beam {
    my ($start_row, $start_col, $start_dir) = @_;
    my %energized;
    my %visited;
    my @queue = [ $start_row, $start_col, $start_dir ];

    while (@queue) {
        my ($row, $col, $dir) = @{shift @queue};

        # Check if out of bounds
        next if $row < 0 || $row >= $rows || $col < 0 || $col >= $cols;

        # Check if this state has been visited
        next if $visited{"$row,$col,$dir"};
        $visited{"$row,$col,$dir"} = 1;

        $energized{"$row,$col"} = 1;

        my $tile = $grid[$row][$col];

        if ($tile eq '.') {
            # Continue in the same direction
            push @queue, [ $row + ($dir eq 'N' ? -1 : $dir eq 'S' ? 1 : 0),
                           $col + ($dir eq 'E' ? 1 : $dir eq 'W' ? -1 : 0),
                           $dir ];
        } elsif ($tile eq '/') {
            # Reflect based on direction
            my $new_dir;
            if ($dir eq 'E') { $new_dir = 'N'; }
            elsif ($dir eq 'W') { $new_dir = 'S'; }
            elsif ($dir eq 'N') { $new_dir = 'E'; }
            elsif ($dir eq 'S') { $new_dir = 'W'; }
            push @queue, [ $row + ($new_dir eq 'N' ? -1 : $new_dir eq 'S' ? 1 : 0),
                           $col + ($new_dir eq 'E' ? 1 : $new_dir eq 'W' ? -1 : 0),
                           $new_dir ];
        } elsif ($tile eq '\\') {
            # Reflect based on direction
            my $new_dir;
            if ($dir eq 'E') { $new_dir = 'S'; }
            elsif ($dir eq 'W') { $new_dir = 'N'; }
            elsif ($dir eq 'N') { $new_dir = 'W'; }
            elsif ($dir eq 'S') { $new_dir = 'E'; }
            push @queue, [ $row + ($new_dir eq 'N' ? -1 : $new_dir eq 'S' ? 1 : 0),
                           $col + ($new_dir eq 'E' ? 1 : $new_dir eq 'W' ? -1 : 0),
                           $new_dir ];
        } elsif ($tile eq '|') {
            if ($dir eq 'N' || $dir eq 'S') {
                # Continue in the same direction
                push @queue, [ $row + ($dir eq 'N' ? -1 : 1), $col, $dir ];
            } else {
                # Split into N and S
                push @queue, [ $row - 1, $col, 'N' ];
                push @queue, [ $row + 1, $col, 'S' ];
            }
        } elsif ($tile eq '-') {
            if ($dir eq 'E' || $dir eq 'W') {
                # Continue in the same direction
                push @queue, [ $row, $col + ($dir eq 'E' ? 1 : -1), $dir ];
            } else {
                # Split into E and W
                push @queue, [ $row, $col + 1, 'E' ];
                push @queue, [ $row, $col - 1, 'W' ];
            }
        }
    }
    return scalar keys %energized;
}

# Simulate the beam starting from top-left going right
my $energized_count = simulate_beam(0, 0, 'E');
print "$energized_count\n";
