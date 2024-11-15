
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

my @DIRS = ([0, -1], [-1, 0], [0, 1], [1, 0]);
my %MIRRORS = (
    '/' => {
        '0,-1' => [1, 0],   # North
        '-1,0' => [0, 1],   # West
        '0,1'  => [-1, 0],  # South
        '1,0'  => [0, -1]   # East
    },
    '\\' => {
        '0,-1' => [-1, 0],  # North
        '-1,0' => [0, -1],  # West
        '0,1'  => [1, 0],   # South
        '1,0'  => [0, 1]    # East
    }
);

sub read_grid {
    open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
    my @grid = map { chomp; [split //] } <$fh>;
    close $fh;
    return \@grid;
}

sub trace_beam {
    my ($grid, $start_x, $start_y, $start_dx, $start_dy) = @_;
    my %energized;
    my %seen;

    my @beams = ([$start_x, $start_y, $start_dx, $start_dy]);

    while (@beams) {
        my ($x, $y, $dx, $dy) = @{shift @beams};
        
        next if $x < 0 || $x >= @{$grid->[0]} || 
                $y < 0 || $y >= @$grid;
        
        my $key = "$x,$y,$dx,$dy";
        next if exists $seen{$key};
        $seen{$key} = 1;
        $energized{"$x,$y"} = 1;

        my $cell = $grid->[$y][$x];
        
        if ($cell eq '/') {
            my $new_dir = $MIRRORS{'/'}{"$dx,$dy"};
            push @beams, [$x + $new_dir->[0], $y + $new_dir->[1], @$new_dir];
        }
        elsif ($cell eq '\\') {
            my $new_dir = $MIRRORS{'\\'}{"$dx,$dy"};
            push @beams, [$x + $new_dir->[0], $y + $new_dir->[1], @$new_dir];
        }
        elsif ($cell eq '|' && $dx != 0) {
            push @beams, 
                [$x, $y - 1, 0, -1],
                [$x, $y + 1, 0, 1];
        }
        elsif ($cell eq '-' && $dy != 0) {
            push @beams, 
                [$x - 1, $y, -1, 0],
                [$x + 1, $y, 1, 0];
        }
        else {
            push @beams, [$x + $dx, $y + $dy, $dx, $dy];
        }
    }

    return scalar keys %energized;
}

sub solve {
    my ($grid) = @_;
    my $max_energy = 0;

    # Top and bottom rows
    for my $x (0 .. $#{$grid->[0]}) {
        $max_energy = max($max_energy, 
            trace_beam($grid, $x, 0, 0, 1),
            trace_beam($grid, $x, $#$grid, 0, -1)
        );
    }

    # Left and right columns
    for my $y (0 .. $#$grid) {
        $max_energy = max($max_energy, 
            trace_beam($grid, 0, $y, 1, 0),
            trace_beam($grid, $#{$grid->[0]}, $y, -1, 0)
        );
    }

    return $max_energy;
}

my $grid = read_grid();
print solve($grid), "\n";
