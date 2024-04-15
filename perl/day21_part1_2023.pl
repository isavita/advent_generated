use strict;
use warnings;

my @input = read_file("input.txt");
my $result = solve(\@input, 64);
print "$result\n";

sub solve {
    my ($input, $num_steps) = @_;
    my $grid = parse_input($input);
    my $start = find_start($grid);
    my $distances = breadth_first_search($grid, $start, \&neighbors4);
    my $cnt = 0;
    foreach my $dist (values %$distances) {
        if ($dist <= $num_steps && $dist % 2 == 0) {
            $cnt++;
        }
    }
    return $cnt;
}

sub read_file {
    my ($file_name) = @_;
    open my $fh, '<', $file_name or die "Could not open file '$file_name': $!";
    my @lines = map { chomp; $_ } <$fh>;
    close $fh;
    return @lines;
}

sub parse_input {
    my ($input) = @_;
    my $grid = {
        Width => length($input->[0]),
        Height => scalar(@$input),
        Data => {},
    };
    for my $y (0 .. $grid->{Height} - 1) {
        for my $x (0 .. $grid->{Width} - 1) {
            my $char = substr($input->[$y], $x, 1);
            if ($char ne '.') {
                $grid->{Data}{"$x,$y"} = $char;
            }
        }
    }
    return $grid;
}

sub find_start {
    my ($grid) = @_;
    foreach my $coord (keys %{$grid->{Data}}) {
        if ($grid->{Data}{$coord} eq 'S') {
            return [split /,/, $coord];
        }
    }
    die "No start found.";
}

sub neighbors4 {
    my ($grid, $coord) = @_;
    my @neighbors = (
        [$coord->[0], $coord->[1] - 1],
        [$coord->[0], $coord->[1] + 1],
        [$coord->[0] + 1, $coord->[1]],
        [$coord->[0] - 1, $coord->[1]],
    );
    my @valid_neighbors;
    foreach my $neighbor (@neighbors) {
        if (is_in_bounds($grid, $neighbor) && !exists $grid->{Data}{"$neighbor->[0],$neighbor->[1]"}) {
            push @valid_neighbors, $neighbor;
        }
    }
    return @valid_neighbors;
}

sub is_in_bounds {
    my ($grid, $coord) = @_;
    return $coord->[0] >= 0 && $coord->[0] < $grid->{Width} &&
           $coord->[1] >= 0 && $coord->[1] < $grid->{Height};
}

sub breadth_first_search {
    my ($grid, $start, $neighbor_func) = @_;
    my @frontier = ($start);
    my %reached = ("$start->[0],$start->[1]" => 1);
    my %distances = ("$start->[0],$start->[1]" => 0);
    while (@frontier) {
        my $current = shift @frontier;
        foreach my $next (&$neighbor_func($grid, $current)) {
            my $next_key = "$next->[0],$next->[1]";
            if (!exists $reached{$next_key}) {
                push @frontier, $next;
                $reached{$next_key} = 1;
                $distances{$next_key} = $distances{"$current->[0],$current->[1]"} + 1;
            }
        }
    }
    return \%distances;
}