use strict;
use warnings;

use constant {
    EMPTY => '.',
    WALL => '#',
    NORTH_SLOPES => '^',
    SOUTH_SLOPES => 'v',
    WEST_SLOPES => '<',
    EAST_SLOPES => '>',
};

my %SLOPE_TO_DIR = (
    NORTH_SLOPES() => [0, -1],
    SOUTH_SLOPES() => [0, 1],
    WEST_SLOPES() => [-1, 0],
    EAST_SLOPES() => [1, 0],
);

sub parse_input {
    my @input = @_;
    my %grid = (
        width => length($input[0]),
        height => scalar(@input),
        data => {},
    );

    for my $y (0 .. $#input) {
        for my $x (0 .. length($input[$y]) - 1) {
            my $char = substr($input[$y], $x, 1);
            if ($char ne EMPTY) {
                $grid{data}{$x}{$y} = $char;
            }
        }
    }

    return \%grid;
}

sub is_valid_neighbor {
    my ($grid, $x, $y) = @_;
    return 0 if $x < 0 || $x >= $grid->{width} || $y < 0 || $y >= $grid->{height};
    return 0 if exists $grid->{data}{$x}{$y} && $grid->{data}{$x}{$y} eq WALL;
    return 1;
}

sub is_valid_neighbor_with_slopes {
    my ($grid, $x, $y, $dir) = @_;
    return 0 if $x < 0 || $x >= $grid->{width} || $y < 0 || $y >= $grid->{height};
    return 1 if !exists $grid->{data}{$x}{$y};
    return 0 if $grid->{data}{$x}{$y} eq WALL;
    return $SLOPE_TO_DIR{$grid->{data}{$x}{$y}}[0] == $dir->[0] && $SLOPE_TO_DIR{$grid->{data}{$x}{$y}}[1] == $dir->[1];
}

sub neighbors4 {
    my ($grid, $x, $y, $is_valid_neighbor_func) = @_;
    my @directions = ([0, -1], [0, 1], [-1, 0], [1, 0]);
    my @valid_neighbors;

    for my $dir (@directions) {
        my ($nx, $ny) = ($x + $dir->[0], $y + $dir->[1]);
        if ($is_valid_neighbor_func->($grid, $nx, $ny, $dir)) {
            push @valid_neighbors, [$nx, $ny];
        }
    }

    return @valid_neighbors;
}

sub get_graph {
    my ($grid, $start_x, $start_y, $end_x, $end_y, $is_valid_neighbor_func) = @_;
    my %graph = (
        vertices => {
            "$start_x,$start_y" => 1,
            "$end_x,$end_y" => 1,
        },
        edges => {},
    );

    for my $y (0 .. $grid->{height} - 1) {
        for my $x (0 .. $grid->{width} - 1) {
            if (!exists $grid->{data}{$x}{$y}) {
                my @neighbors = neighbors4($grid, $x, $y, \&is_valid_neighbor);
                if (scalar(@neighbors) > 2) {
                    $graph{vertices}{"$x,$y"} = 1;
                }
            }
        }
    }

    for my $start (keys %{$graph{vertices}}) {
        my ($start_x, $start_y) = split /,/, $start;
        my %edges = get_edges_bfs($grid, $start_x, $start_y, \%{$graph{vertices}}, $is_valid_neighbor_func);
        $graph{edges}{$start} = \%edges;
    }

    return \%graph;
}

sub get_edges_bfs {
    my ($grid, $start_x, $start_y, $vertices, $is_valid_neighbor_func) = @_;
    my @frontier = ([$start_x, $start_y]);
    my %reached = ("$start_x,$start_y" => 1);
    my %distances = ("$start_x,$start_y" => 0);
    my %edges;

    while (scalar(@frontier) > 0) {
        my ($x, $y) = @{shift @frontier};
        my $current = "$x,$y";

        if (exists $vertices->{$current} && $current ne "$start_x,$start_y") {
            $edges{$current} = $distances{$current};
            next;
        }

        for my $next (neighbors4($grid, $x, $y, $is_valid_neighbor_func)) {
            my ($nx, $ny) = @$next;
            my $next_key = "$nx,$ny";
            if (!exists $reached{$next_key}) {
                push @frontier, $next;
                $reached{$next_key} = 1;
                $distances{$next_key} = $distances{$current} + 1;
            }
        }
    }

    return %edges;
}

sub get_max_distance_dfs {
    my ($grid, $graph, $current_x, $current_y, $end_x, $end_y, $seen) = @_;
    my $current = "$current_x,$current_y";
    my $end = "$end_x,$end_y";

    if ($current eq $end) {
        return (1, 0);
    }

    my $max_dist = 0;
    $seen->{$current} = 1;
    for my $edge (keys %{$graph->{edges}{$current}}) {
        my ($nx, $ny) = split /,/, $edge;
        if (!exists $seen->{$edge}) {
            my ($is_valid, $dist) = get_max_distance_dfs($grid, $graph, $nx, $ny, $end_x, $end_y, $seen);
            if ($is_valid) {
                $max_dist = $max_dist > $dist + $graph->{edges}{$current}{$edge} ? $max_dist : $dist + $graph->{edges}{$current}{$edge};
            }
        }
    }
    delete $seen->{$current};

    if ($max_dist == 0) {
        return (0, 0);
    }
    return (1, $max_dist);
}

sub solve {
    my @input = @_;
    my $grid = parse_input(@input);

    my $start_x = 1;
    my $start_y = 0;
    my $end_x = $grid->{width} - 2;
    my $end_y = $grid->{height} - 1;

    my $graph = get_graph($grid, $start_x, $start_y, $end_x, $end_y, \&is_valid_neighbor_with_slopes);

    my ($is_valid, $max_dist) = get_max_distance_dfs($grid, $graph, $start_x, $start_y, $end_x, $end_y, {});
    return $max_dist;
}

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @input = <$fh>;
chomp @input;
close $fh;

my $result = solve(@input);
print "$result\n";