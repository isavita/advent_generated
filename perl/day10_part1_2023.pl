
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(first);

# Read input file
open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
my @input = <$fh>;
chomp @input;
close $fh;

# Constants
use constant {
    EMPTY => '.',
    START => 'S',
    VERTICAL => '|',
    HORIZONTAL => '-',
    TOP_LEFT => 'J',
    TOP_RIGHT => 'L',
    BOTTOM_LEFT => '7',
    BOTTOM_RIGHT => 'F'
};

# Pipe connection directions
my %PIPES = (
    VERTICAL() => {top => 1, bottom => 1},
    HORIZONTAL() => {left => 1, right => 1},
    TOP_LEFT() => {top => 1, left => 1},
    TOP_RIGHT() => {top => 1, right => 1},
    BOTTOM_LEFT() => {bottom => 1, left => 1},
    BOTTOM_RIGHT() => {bottom => 1, right => 1}
);

# Directions
my %DIRS = (
    top => [0, -1],
    right => [1, 0],
    bottom => [0, 1],
    left => [-1, 0]
);

sub find_start {
    my ($grid) = @_;
    for my $y (0..$#$grid) {
        for my $x (0..length($grid->[$y])-1) {
            return [$x, $y] if substr($grid->[$y], $x, 1) eq START;
        }
    }
    return undef;
}

sub get_connections {
    my ($grid, $start) = @_;
    my $connections = {};
    
    for my $dir (keys %DIRS) {
        my ($dx, $dy) = @{$DIRS{$dir}};
        my ($nx, $ny) = ($start->[0] + $dx, $start->[1] + $dy);
        
        next if $nx < 0 || $ny < 0 || $ny >= @$grid || $nx >= length($grid->[$ny]);
        
        my $tile = substr($grid->[$ny], $nx, 1);
        my $pipe = $PIPES{$tile} || {};
        
        my $opposite = {
            top => 'bottom', 
            bottom => 'top', 
            left => 'right', 
            right => 'left'
        }->{$dir};
        
        $connections->{$dir} = 1 if exists $pipe->{$opposite};
    }
    
    return $connections;
}

sub find_path {
    my ($grid, $start) = @_;
    my @path = ($start);
    my $connections = get_connections($grid, $start);
    
    my $current = $start;
    my $prev_dir;
    
    for my $dir (keys %$connections) {
        $prev_dir = $dir;
        my ($dx, $dy) = @{$DIRS{$dir}};
        $current = [$current->[0] + $dx, $current->[1] + $dy];
        last;
    }
    
    while (!($current->[0] == $start->[0] && $current->[1] == $start->[1])) {
        push @path, $current;
        
        my $tile = substr($grid->[$current->[1]], $current->[0], 1);
        my $pipe = $PIPES{$tile};
        
        for my $dir (keys %$pipe) {
            my $opposite = {
                top => 'bottom', 
                bottom => 'top', 
                left => 'right', 
                right => 'left'
            }->{$prev_dir};
            
            next if $dir eq $opposite;
            
            my ($dx, $dy) = @{$DIRS{$dir}};
            $prev_dir = $dir;
            $current = [$current->[0] + $dx, $current->[1] + $dy];
            last;
        }
    }
    
    return scalar(@path) / 2;
}

# Main solve function
sub solve {
    my ($input) = @_;
    my $start = find_start($input);
    return find_path($input, $start);
}

# Solve and print result
print solve(\@input), "\n";
