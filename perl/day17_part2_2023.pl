#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

open(my $fh, "<", "input.txt") or die $!;
chomp(my @lines = <$fh>);
close($fh);
my $W = length($lines[0]);
my $H = scalar @lines;
my %grid;
for my $y (0..$H-1) {
    my @chars = split('', $lines[$y]);
    for my $x (0..$W-1) {
        $grid{"$x,$y"} = $chars[$x] + 0;
    }
}
sub in_bounds {
    my ($x, $y) = @_;
    return $x >= 0 && $x < $W && $y >= 0 && $y < $H;
}
sub neighbors {
    my ($x, $y) = @_;
    return ([$x, $y-1], [$x-1, $y], [$x, $y+1], [$x+1, $y]);
}
sub heuristic {
    my ($x,$y,$gx,$gy) = @_;
    return abs($x - $gx) + abs($y - $gy);
}
my $minStraight = 4;
my $maxStraight = 10;
my $goalX = $W - 1;
my $goalY = $H - 1;

my @heap;
sub heap_push {
    my ($item) = @_;
    push @heap, $item;
    my $i = $#heap;
    while ($i > 0) {
        my $p = int(($i-1)/2);
        if ($heap[$i][0] < $heap[$p][0]) {
            @heap[$i,$p] = @heap[$p,$i];
            $i = $p;
        } else { last }
    }
}
sub heap_pop {
    return undef unless @heap;
    my $ret = $heap[0];
    my $last = pop @heap;
    if (@heap) {
        $heap[0] = $last;
        my $i = 0;
        my $n = scalar @heap;
        while (1) {
            my $l = 2*$i+1;
            my $r = 2*$i+2;
            my $small = $i;
            if ($l < $n and $heap[$l][0] < $heap[$small][0]) { $small = $l; }
            if ($r < $n and $heap[$r][0] < $heap[$small][0]) { $small = $r; }
            last if $small == $i;
            @heap[$i,$small] = @heap[$small,$i];
            $i = $small;
        }
    }
    return $ret;
}
my %cost; 
my $start_key = "0,0,0,0,0";
$cost{$start_key} = 0;
heap_push([heuristic(0,0,$goalX,$goalY), 0, 0, 0, 0, 0, 0]);
while (defined(my $curr = heap_pop())) {
    my ($pri, $g, $x, $y, $dx, $dy, $st) = @$curr;
    last if $x == $goalX && $y == $goalY;
    for my $nb (neighbors($x,$y)) {
        my ($nx, $ny) = @$nb;
        next unless in_bounds($nx,$ny);
        my $ndx = $nx - $x;
        my $ndy = $ny - $y;
        my $nst = ($ndx == $dx && $ndy == $dy) ? $st + 1 : 1;
        my $curStart = ($x==0 && $y==0);
        my $validStraight = ($curStart || $st >= $minStraight || ($ndx==$dx && $ndy==$dy)) && ($nst <= $maxStraight);
        my $opp = (-$dx, -$dy);
        my $notOpp = !($ndx == -$dx && $ndy == -$dy);
        next unless ($validStraight && $notOpp);
        my $newCost = $g + $grid{"$nx,$ny"};
        my $state_key = "$nx,$ny,$ndx,$ndy,$nst";
        if (!defined($cost{$state_key}) || $newCost < $cost{$state_key}) {
            $cost{$state_key} = $newCost;
            my $priority = $newCost + heuristic($nx,$ny,$goalX,$goalY);
            heap_push([$priority, $newCost, $nx, $ny, $ndx, $ndy, $nst]);
        }
    }
}
my $ans = 1e9;
for my $k (keys %cost) {
    my ($x,$y) = split /,/, $k;
    if ($x==$goalX && $y==$goalY) {
        $ans = min($ans, $cost{$k});
    }
}
print($ans, "\n");