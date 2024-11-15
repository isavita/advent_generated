
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

my @DIRS = ([0, -1], [-1, 0], [0, 1], [1, 0]);
my ($EMPTY, $CUBIC_ROCK, $ROUND_ROCK) = ('.', '#', 'O');

sub read_input {
    open my $fh, '<', 'input.txt' or die "Cannot open file: $!";
    my @input = map { chomp; $_ } <$fh>;
    close $fh;
    return \@input;
}

sub build_grid {
    my ($input) = @_;
    my %grid;
    my $height = scalar @$input;
    my $width = length $input->[0];

    for my $y (0..$height-1) {
        for my $x (0..$width-1) {
            my $char = substr($input->[$y], $x, 1);
            $grid{"$x,$y"} = $char if $char ne $EMPTY;
        }
    }

    return { width => $width, height => $height, data => \%grid };
}

sub is_in_bounds {
    my ($grid, $x, $y) = @_;
    return $x >= 0 && $x < $grid->{width} && $y >= 0 && $y < $grid->{height};
}

sub shift_single_rock {
    my ($grid, $x, $y, $dx, $dy) = @_;
    my $key = "$x,$y";
    return unless exists $grid->{data}{$key} && $grid->{data}{$key} eq $ROUND_ROCK;

    my ($nx, $ny) = ($x + $dx, $y + $dy);
    my $new_key = "$nx,$ny";

    while (is_in_bounds($grid, $nx, $ny) && !exists $grid->{data}{$new_key}) {
        $grid->{data}{$new_key} = $ROUND_ROCK;
        delete $grid->{data}{$key};
        
        ($x, $y) = ($nx, $ny);
        ($key, $new_key) = ($new_key, sprintf("%d,%d", $nx + $dx, $ny + $dy));
        ($nx, $ny) = ($nx + $dx, $ny + $dy);
    }
}

sub shift_rocks {
    my ($grid, $dx, $dy) = @_;
    my @xs = $dx >= 0 ? (reverse 0..$grid->{width}-1) : (0..$grid->{width}-1);
    my @ys = $dy >= 0 ? (reverse 0..$grid->{height}-1) : (0..$grid->{height}-1);

    for my $x (@xs) {
        for my $y (@ys) {
            shift_single_rock($grid, $x, $y, $dx, $dy);
        }
    }
}

sub cycle_rocks {
    my ($grid) = @_;
    for my $dir (@DIRS) {
        shift_rocks($grid, $dir->[0], $dir->[1]);
    }
}

sub calculate_grid_key {
    my ($grid) = @_;
    return sum(map { 
        my ($x, $y) = split ',', $_;
        $x + $y * $grid->{width}
    } grep { $grid->{data}{$_} eq $ROUND_ROCK } keys %{$grid->{data}}) || 0;
}

sub calculate_load {
    my ($grid) = @_;
    return sum(map { 
        my ($x, $y) = split ',', $_;
        $grid->{height} - $y
    } grep { $grid->{data}{$_} eq $ROUND_ROCK } keys %{$grid->{data}}) || 0;
}

sub solve {
    my ($input) = @_;
    my $grid = build_grid($input);
    my $num_cycles = 1_000_000_000;
    my %cache;

    for my $i (0..$num_cycles-1) {
        my $grid_key = calculate_grid_key($grid);
        
        if (exists $cache{$grid_key}) {
            my $cycle_length = $i - $cache{$grid_key};
            my $remaining_cycles = ($num_cycles - $i) % $cycle_length;
            
            for (1..$remaining_cycles) {
                cycle_rocks($grid);
            }
            
            return calculate_load($grid);
        }
        
        $cache{$grid_key} = $i;
        cycle_rocks($grid);
    }

    return calculate_load($grid);
}

my $input = read_input();
print solve($input), "\n";
