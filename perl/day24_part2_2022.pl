#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min max);
use POSIX qw(floor);
open(my $fh, '<', 'input.txt') or die $!;
chomp(my @lines = <$fh>);
close($fh);
my (%grid, @points);
for my $y (0..$#lines) {
    my @chars = split //, $lines[$y];
    for my $x (0..$#chars) {
        my $c = $chars[$x];
        if($c ne "."){
            $grid{"$x,$y"} = $c;
            push @points, [$x,$y];
        }
    }
}
my ($min_x, $min_y, $max_x, $max_y);
($min_x, $min_y) = ($points[0][0], $points[0][1]);
($max_x, $max_y) = ($points[0][0]+1, $points[0][1]+1);
for (@points) {
    my ($x,$y)=@$_;
    $min_x = min($min_x,$x);
    $min_y = min($min_y,$y);
    $max_x = max($max_x,$x+1);
    $max_y = max($max_y,$y+1);
}
my $entrance = [1,0];
my $exit = [$max_x-2, $max_y-1];
my $inner_base_x = 1;
my $inner_base_y = 1;
my $inner_w = $max_x - 2;
my $inner_h = $max_y - 2;
sub gcd {
    my ($a,$b) = @_;
    ($a,$b) = ($b, $a % $b) while $b;
    return $a;
}
sub lcm {
    my ($a,$b) = @_;
    return ($a && $b) ? $a*$b/gcd($a,$b) : 0;
}
my $period = lcm($inner_w, $inner_h);
sub in_bounds {
    my ($x,$y) = @_;
    return ($x >= $min_x && $x < $max_x && $y >= $min_y && $y < $max_y);
}
my %blizzard_vec = (
    '^' => [ 0, -1],
    '>' => [ 1,  0],
    'v' => [ 0,  1],
    '<' => [-1,  0],
);
sub mod_inner {
    my ($x, $y) = @_;
    my $nx = (($x - $inner_base_x) % $inner_w + $inner_w) % $inner_w + $inner_base_x;
    my $ny = (($y - $inner_base_y) % $inner_h + $inner_h) % $inner_h + $inner_base_y;
    return ($nx, $ny);
}
sub steps {
    my ($start, $end, $init_step) = @_;
    my @queue = ( [$start->[0], $start->[1], $init_step] );
    my %seen;
    while(@queue) {
        my ($x,$y,$step) = @{shift @queue};
        return $step if ($x==$end->[0] && $y==$end->[1]);
        for my $d ([0,1],[0,-1],[1,0],[-1,0],[0,0]) {
            my ($nx,$ny) = ($x+$d->[0], $y+$d->[1]);
            my $nstep = $step+1;
            next unless in_bounds($nx,$ny);
            next if (exists $grid{"$nx,$ny"} && $grid{"$nx,$ny"} eq '#');
            if($ny > 0 && $ny < $max_y-1) {
                my $skip = 0;
                for my $b (qw(^ > v <)) {
                    my $vec = $blizzard_vec{$b};
                    my $tx = $nx - $vec->[0]*$nstep;
                    my $ty = $ny - $vec->[1]*$nstep;
                    ($tx, $ty) = mod_inner($tx, $ty);
                    if(defined $grid{"$tx,$ty"} && $grid{"$tx,$ty"} eq $b) { $skip=1; last; }
                }
                next if $skip;
            }
            my $key = "$nx,$ny,".($nstep % $period);
            next if $seen{$key}++;
            push @queue, [$nx,$ny,$nstep];
        }
    }
    return -1;
}
my $first = steps($entrance, $exit, 0);
my $second = steps($exit, $entrance, $first);
my $third = steps($entrance, $exit, $second);
print "$third\n";