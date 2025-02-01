#!/usr/bin/perl
use strict;
use warnings;
use Carp;
local $/ = undef;
my $data = do { open my $fh, "<", "input.txt" or die $!; <$fh> };
my @lines = split /\n/, $data;
my (%nodes, $maxx, $maxy) = ((), 0, 0);
for my $line (@lines[2..$#lines]) {
    my @f = split ' ', $line;
    $f[2] =~ s/T//;
    $f[3] =~ s/T//;
    $line =~ /-x(\d+)-y(\d+)/ or next;
    my ($x, $y) = ($1, $2);
    $nodes{"$x,$y"} = { used => $f[2] + 0, avail => $f[3] + 0 };
    $maxx = $x if $x > $maxx;
    $maxy = $y if $y > $maxy;
}
print minmoves(\%nodes, $maxx, $maxy), "\n";

sub minmoves {
    my ($nodes, $w, $h) = @_;
    my $goal = "$w,0";
    my $hole = findHole($nodes);
    my $sum = 0;
    while ($goal ne "0,0") {
        my ($gx, $gy) = split /,/, $goal;
        my $next = ($gx - 1) . ",$gy";
        $sum += moves($nodes, $goal, $hole, $next, $w, $h);
        $hole = $next;
        $sum += moves($nodes, $goal, $goal, $hole, $w, $h);
        ($goal, $hole) = ($hole, $goal);
    }
    return $sum;
}

sub findHole {
    my ($nodes) = @_;
    for my $p (keys %$nodes) {
        return $p if $nodes->{$p}->{used} == 0;
    }
    croak "no hole";
}

sub moves {
    my ($nodes, $goal, $from, $to, $w, $h) = @_;
    my %dist = ($from => 0);
    my @heap;
    heap_push(\@heap, [0, $from]);
    my @nb = ([0,1], [0,-1], [1,0], [-1,0]);
    while (@heap) {
        my ($d, $pos) = @{ heap_pop(\@heap) };
        return $d if $pos eq $to;
        my ($x, $y) = split /,/, $pos;
        my $nd = $d + 1;
        for my $delta (@nb) {
            my $nx = $x + $delta->[0];
            my $ny = $y + $delta->[1];
            next if $nx < 0 || $ny < 0 || $nx > $w || $ny > $h;
            my $npos = "$nx,$ny";
            next if (exists $nodes->{$npos} && $nodes->{$npos}->{used} > 400);
            next if $npos eq $goal;
            if (!defined($dist{$npos}) or $nd < $dist{$npos}) {
                $dist{$npos} = $nd;
                heap_push(\@heap, [$nd, $npos]);
            }
        }
    }
    croak "no possible path";
}

sub heap_push {
    my ($heap, $item) = @_;
    push @$heap, $item;
    my $i = $#$heap;
    while ($i > 0) {
        my $p = int(($i - 1) / 2);
        last if $heap->[$i]->[0] >= $heap->[$p]->[0];
        @$heap[$i, $p] = @$heap[$p, $i];
        $i = $p;
    }
}

sub heap_pop {
    my ($heap) = @_;
    return unless @$heap;
    my $min = $heap->[0];
    my $last = pop @$heap;
    if (@$heap) {
        $heap->[0] = $last;
        my $i = 0;
        my $n = scalar @$heap;
        while (1) {
            my $l = 2 * $i + 1;
            my $r = 2 * $i + 2;
            my $small = $i;
            $small = $l if $l < $n && $heap->[$l]->[0] < $heap->[$small]->[0];
            $small = $r if $r < $n && $heap->[$r]->[0] < $heap->[$small]->[0];
            last if $small == $i;
            @$heap[$i, $small] = @$heap[$small, $i];
            $i = $small;
        }
    }
    return $min;
}