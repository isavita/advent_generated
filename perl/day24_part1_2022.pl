#!/usr/bin/perl
use strict;
use warnings;
use integer;
use List::Util qw(min max);

open(my $fh, "<", "input.txt") or die $!;
my @lines = <$fh>;
close $fh;
chomp @lines;
my %grid;
my ($minx,$miny,$maxx,$maxy);
$minx = 1e9; $miny = 1e9; $maxx = -1e9; $maxy = -1e9;
for my $y (0..$#lines) {
    my @chars = split //, $lines[$y];
    for my $x (0..$#chars) {
        my $c = $chars[$x];
        if($c ne "."){
            $grid{"$x,$y"} = $c;
            $minx = $x if $x < $minx;
            $miny = $y if $y < $miny;
            $maxx = $x if $x > $maxx;
            $maxy = $y if $y > $maxy;
        }
    }
}
# Bounds: rectangle from (minx, miny) to (maxx+1, maxy+1)
my $Bminx = $minx;
my $Bminy = $miny;
my $Bmaxx = $maxx + 1;
my $Bmaxy = $maxy + 1;
# Inner area is bounds inset by 1
my $Iminx = $Bminx + 1;
my $Iminy = $Bminy + 1;
my $Imaxx = $Bmaxx - 1;
my $Imaxy = $Bmaxy - 1;
my $inner_width  = $Imaxx - $Iminx;
my $inner_height = $Imaxy - $Iminy;
sub gcd {
    my ($a,$b)=@_;
    ($b==0)? $a : gcd($b, $a % $b);
}
sub lcm {
    my ($a,$b)=@_;
    return ($a && $b) ? ($a*$b)/gcd($a,$b) : 0;
}
my $period = lcm($inner_width, $inner_height) || 1;
# Entrance and exit
my ($sx, $sy) = (1, 0);
my ($ex, $ey) = ($Bmaxx - 2, $Bmaxy - 1);
# Moves: stay, right, left, down, up
my @moves = ([0,0],[1,0],[-1,0],[0,1],[0,-1]);
# Blizzard reversed vectors for '^','>','v','<'
my %bliz_rev = (
    '^' => [ 0, -1 ],
    '>' => [ 1,  0 ],
    'v' => [ 0,  1 ],
    '<' => [-1,  0 ],
);
# mod function for positive mod
sub modp {
    my ($a,$m)=@_;
    return (($a % $m) + $m) % $m;
}
my @queue = ( { x=>$sx, y=>$sy, t=>0 } );
my %seen;
my $head = 0;
while($head < @queue){
    my $state = $queue[$head++];
    my ($x,$y,$t) = ($state->{x}, $state->{y}, $state->{t});
    if($x==$ex && $y==$ey){
        print "$t\n";
        exit;
    }
    my $nt = $t + 1;
    for my $m (@moves) {
        my ($dx,$dy) = @$m;
        my $nx = $x + $dx;
        my $ny = $y + $dy;
        # Check if within overall bounds
        next if $nx < $Bminx || $nx >= $Bmaxx;
        next if $ny < $Bminy || $ny >= $Bmaxy;
        # Wall check
        next if (exists $grid{"$nx,$ny"} and $grid{"$nx,$ny"} eq "#");
        # Use periodic seen key for states within moving area; outside (entrance/exit) not affected.
        my $key = "$nx,$ny," . ($nt % $period);
        next if $seen{$key}++;
        # Blizzard check for positions not in the outer row
        if($ny > 0 and $ny < $Bmaxy-1){
            my $unsafe = 0;
            for my $b (qw(^ > v <)) {
                my ($rdx,$rdy) = @{$bliz_rev{$b}};
                my $orig_x = $Iminx + modp( ($nx - $Iminx) - $rdx * $nt, $inner_width );
                my $orig_y = $Iminy + modp( ($ny - $Iminy) - $rdy * $nt, $inner_height );
                if( exists $grid{"$orig_x,$orig_y"} and $grid{"$orig_x,$orig_y"} eq $b ){
                    $unsafe = 1;
                    last;
                }
            }
            next if $unsafe;
        }
        push @queue, { x => $nx, y => $ny, t => $nt };
    }
}
print "-1\n";