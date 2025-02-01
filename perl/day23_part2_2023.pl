#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(max);

my $input_file = "input.txt";
open(my $fh, "<", $input_file) or die $!;
chomp(my @lines = <$fh>);
close($fh);

my $grid = {
    height => scalar(@lines),
    width  => length($lines[0]),
    data   => {},
};
for my $y (0..$#lines) {
    my @chars = split //, $lines[$y];
    for my $x (0..$#chars) {
        $grid->{data}{"$x,$y"} = $chars[$x] if $chars[$x] ne '.';
    }
}

sub in_bounds {
    my ($g, $coord) = @_;
    my ($x, $y) = @$coord;
    return $x >= 0 && $x < $g->{width} && $y >= 0 && $y < $g->{height};
}

sub is_valid {
    my ($g, $coord, $dir) = @_;
    return 0 unless in_bounds($g, $coord);
    return 0 if exists $g->{data}{"$coord->[0],$coord->[1]"} && $g->{data}{"$coord->[0],$coord->[1]"} eq '#';
    return 1;
}

my @dirs = ([0,-1],[0,1],[-1,0],[1,0]);

sub neighbors4 {
    my ($g, $coord, $fn) = @_;
    my @n;
    for my $d (@dirs) {
        my $ncoord = [$coord->[0] + $d->[0], $coord->[1] + $d->[1]];
        push @n, $ncoord if $fn->($g, $ncoord, $d);
    }
    return @n;
}

sub get_edges_bfs {
    my ($g, $start, $vertices, $fn) = @_;
    my @queue = ($start);
    my %reached = ( "$start->[0],$start->[1]" => 1 );
    my %dist = ( "$start->[0],$start->[1]" => 0 );
    my @edges;
    my $start_key = "$start->[0],$start->[1]";
    while (@queue) {
        my $curr = shift @queue;
        my $ckey = "$curr->[0],$curr->[1]";
        if (exists $vertices->{$ckey} && $ckey ne $start_key) {
            push @edges, { end => $curr, weight => $dist{$ckey} };
            next;
        }
        for my $nb (neighbors4($g, $curr, $fn)) {
            my $nkey = "$nb->[0],$nb->[1]";
            next if $reached{$nkey};
            $reached{$nkey} = 1;
            $dist{$nkey} = $dist{$ckey} + 1;
            push @queue, $nb;
        }
    }
    return \@edges;
}

sub get_graph {
    my ($g, $start, $end, $fn) = @_;
    my %vertices;
    $vertices{"$start->[0],$start->[1]"} = 1;
    $vertices{"$end->[0],$end->[1]"} = 1;
    for my $y (0..$g->{height}-1) {
        for my $x (0..$g->{width}-1) {
            my $key = "$x,$y";
            next if exists $g->{data}{$key};
            my @nb = neighbors4($g, [$x,$y], \&is_valid);
            $vertices{$key} = 1 if @nb > 2;
        }
    }
    my %edges;
    for my $v_key (keys %vertices) {
        my ($vx, $vy) = split /,/, $v_key;
        my $v = [$vx, $vy];
        $edges{$v_key} = get_edges_bfs($g, $v, \%vertices, $fn);
    }
    return { vertices => \%vertices, edges => \%edges };
}

sub dfs_max {
    my ($graph, $curr, $end, $seen) = @_;
    my $curr_key = "$curr->[0],$curr->[1]";
    return (1,0) if $curr_key eq "$end->[0],$end->[1]";
    $seen->{$curr_key} = 1;
    my $maxd = 0;
    for my $edge (@{ $graph->{edges}{$curr_key} || [] }) {
        my $nkey = "$edge->{end}->[0],$edge->{end}->[1]";
        next if $seen->{$nkey};
        my ($ok, $d) = dfs_max($graph, $edge->{end}, $end, $seen);
        $maxd = max($maxd, $d + $edge->{weight}) if $ok;
    }
    delete $seen->{$curr_key};
    return ($maxd ? (1, $maxd) : (0,0));
}

sub solve {
    my $g = shift;
    my $start = [1,0];
    my $end = [$g->{width}-2, $g->{height}-1];
    my $graph = get_graph($g, $start, $end, \&is_valid);
    my (undef, $maxd) = dfs_max($graph, $start, $end, {});
    return $maxd;
}

print solve($grid), "\n";