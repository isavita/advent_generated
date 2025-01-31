
#!/usr/bin/perl

use strict;
use warnings;

my $filename = "input.txt";
my $grid_size = 71; 

sub read_input {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Could not open file '$filename' $!";
    my @bytes;
    while (my $line = <$fh>) {
        chomp $line;
        my ($x, $y) = split(',', $line);
        push @bytes, [$x, $y];
    }
    close $fh;
    return @bytes;
}

sub is_valid {
    my ($x, $y, $grid) = @_;
    return ($x >= 0 && $x < $grid_size && $y >= 0 && $y < $grid_size && !$grid->[$y][$x]);
}

sub solve_part1 {
    my (@bytes) = @_;
    my @grid;
    for (my $i = 0; $i < $grid_size; $i++) {
        for (my $j = 0; $j < $grid_size; $j++) {
            $grid[$i][$j] = 0;
        }
    }

    my $steps = 0;
    for (my $i = 0; $i < 1024; $i++) {
        $grid[$bytes[$i]->[1]][$bytes[$i]->[0]] = 1;
    }
   
    my @queue = ([0, 0, 0]); 
    my %visited;
    $visited{"0,0"} = 1;

    while (@queue) {
        my ($x, $y, $dist) = @{shift @queue};

        if ($x == $grid_size - 1 && $y == $grid_size - 1) {
            $steps = $dist;
            last;
        }

        my @moves = ([-1, 0], [1, 0], [0, -1], [0, 1]);
        for my $move (@moves) {
            my $nx = $x + $move->[0];
            my $ny = $y + $move->[1];
            if (is_valid($nx, $ny, \@grid) && !$visited{"$nx,$ny"}) {
                push @queue, [$nx, $ny, $dist + 1];
                $visited{"$nx,$ny"} = 1;
            }
        }
    }
    return $steps;
}

sub solve_part2 {
    my (@bytes) = @_;
    my @grid;

    for (my $byte_index = 0; $byte_index < scalar @bytes; $byte_index++) {
        for (my $i = 0; $i < $grid_size; $i++) {
            for (my $j = 0; $j < $grid_size; $j++) {
                $grid[$i][$j] = 0;
            }
        }
        for(my $i = 0; $i <= $byte_index; $i++) {
            $grid[$bytes[$i]->[1]][$bytes[$i]->[0]] = 1;
        }

        my @queue = ([0, 0]);
        my %visited;
        $visited{"0,0"} = 1;
        my $path_exists = 0;

        while (@queue) {
            my ($x, $y) = @{shift @queue};

            if ($x == $grid_size - 1 && $y == $grid_size - 1) {
                $path_exists = 1;
                last;
            }

            my @moves = ([-1, 0], [1, 0], [0, -1], [0, 1]);
            for my $move (@moves) {
                my $nx = $x + $move->[0];
                my $ny = $y + $move->[1];

                if (is_valid($nx, $ny, \@grid) && !$visited{"$nx,$ny"}) {
                    push @queue, [$nx, $ny];
                    $visited{"$nx,$ny"} = 1;
                }
            }
        }
        if (!$path_exists) {
            return "$bytes[$byte_index]->[0],$bytes[$byte_index]->[1]";
        }
    }

    return "";
}

my @bytes = read_input($filename);
my $part1_solution = solve_part1(@bytes);
my $part2_solution = solve_part2(@bytes);

print "Part 1: $part1_solution\n";
print "Part 2: $part2_solution\n";
