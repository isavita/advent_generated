
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(first);
use Data::Dumper;

# Constants
use constant {
    EMPTY => '.',
    START => 'S',
    VERTICAL => '|',
    HORIZONTAL => '-',
    TOP_LEFT => 'J',
    TOP_RIGHT => 'L',
    BOTTOM_LEFT => '7',
    BOTTOM_RIGHT => 'F',
    ENCLOSED => 'X',
};

# Directions
my %DIRS = (
    TOP => [0, -1],
    RIGHT => [1, 0],
    BOTTOM => [0, 1],
    LEFT => [-1, 0],
);

# Pipe connections
my %PIPES = (
    VERTICAL() => { TOP => 1, BOTTOM => 1 },
    HORIZONTAL() => { LEFT => 1, RIGHT => 1 },
    TOP_LEFT() => { TOP => 1, LEFT => 1 },
    TOP_RIGHT() => { TOP => 1, RIGHT => 1 },
    BOTTOM_LEFT() => { BOTTOM => 1, LEFT => 1 },
    BOTTOM_RIGHT() => { BOTTOM => 1, RIGHT => 1 },
);

# Read input file
open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @input = map { chomp; $_ } <$fh>;
close $fh;

sub build_grid {
    my @input = @_;
    my %grid;
    for my $y (0..$#input) {
        for my $x (0..length($input[$y])-1) {
            my $char = substr($input[$y], $x, 1);
            $grid{"$x,$y"} = $char if $char ne EMPTY;
        }
    }
    return { 
        width => length($input[0]), 
        height => scalar(@input), 
        data => \%grid 
    };
}

sub find_start {
    my ($grid) = @_;
    for my $coord (keys %{$grid->{data}}) {
        return $coord if $grid->{data}{$coord} eq START;
    }
    return undef;
}

sub add_coord {
    my ($coord, $dir) = @_;
    my ($x, $y) = split(',', $coord);
    my ($dx, $dy) = @{$DIRS{$dir}};
    return sprintf("%d,%d", $x + $dx, $y + $dy);
}

sub get_pipe_from_neighbors {
    my ($grid, $start) = @_;
    my %pipe;
    for my $dir (keys %DIRS) {
        my $neighbor = add_coord($start, $dir);
        my $opposite = { TOP => 'BOTTOM', RIGHT => 'LEFT', BOTTOM => 'TOP', LEFT => 'RIGHT' }->{$dir};
        if (exists $grid->{data}{$neighbor} && 
            exists $PIPES{$grid->{data}{$neighbor}}{$opposite}) {
            $pipe{$dir} = 1;
        }
    }
    return \%pipe;
}

sub path_finding {
    my ($grid, $start) = @_;
    my @path = ($start);
    my $start_pipe = get_pipe_from_neighbors($grid, $start);
    
    my ($prev_dir) = keys %$start_pipe;
    my $current = add_coord($start, $prev_dir);
    
    while ($current ne $start) {
        push @path, $current;
        my $current_pipe = $PIPES{$grid->{data}{$current}};
        for my $dir (keys %$current_pipe) {
            my $next = add_coord($current, $dir);
            if ($next ne $path[-2]) {
                $current = $next;
                last;
            }
        }
    }
    
    return \@path;
}

sub is_inside {
    my ($coord, $path_grid) = @_;
    return 0 if exists $path_grid->{$coord};
    
    my ($x, $y) = split(',', $coord);
    my $crossings = 0;
    my $start_pipe = undef;
    
    for my $i (0..$x-1) {
        my $check_coord = "$i,$y";
        next unless exists $path_grid->{$check_coord};
        
        my $tile = $path_grid->{$check_coord};
        if ($tile eq VERTICAL) {
            $crossings++;
        } elsif ($tile eq TOP_RIGHT) {
            $start_pipe = TOP_RIGHT;
        } elsif ($tile eq BOTTOM_RIGHT) {
            $start_pipe = BOTTOM_RIGHT;
        } elsif ($tile eq TOP_LEFT) {
            if ($start_pipe && $start_pipe eq BOTTOM_RIGHT) {
                $start_pipe = undef;
                $crossings++;
            } elsif ($start_pipe && $start_pipe eq TOP_RIGHT) {
                $start_pipe = undef;
            }
        } elsif ($tile eq BOTTOM_LEFT) {
            if ($start_pipe && $start_pipe eq TOP_RIGHT) {
                $start_pipe = undef;
                $crossings++;
            } elsif ($start_pipe && $start_pipe eq BOTTOM_RIGHT) {
                $start_pipe = undef;
            }
        }
    }
    
    return $crossings % 2 == 1;
}

sub solve {
    my (@input) = @_;
    my $grid = build_grid(@input);
    my $start = find_start($grid);
    my $path = path_finding($grid, $start);
    
    my %path_grid = map { $_ => $grid->{data}{$_} } @$path;
    
    my $cnt = 0;
    for my $y (0..$grid->{height}-1) {
        for my $x (0..$grid->{width}-1) {
            my $coord = "$x,$y";
            $cnt++ if is_inside($coord, \%path_grid);
        }
    }
    
    return $cnt;
}

print solve(@input), "\n";
