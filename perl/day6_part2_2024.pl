#!/usr/bin/perl
use strict;
use warnings;
open(my $fh, "<", "input.txt") or die $!;
my @grid = map { chomp; [split //] } <$fh>;
close($fh);
my $h = scalar @grid;
my $w = scalar @{$grid[0]};
my ($sx,$sy,$sd);
for my $y (0..$h-1) {
    for my $x (0..$w-1) {
        my $c = $grid[$y][$x];
        if($c eq '^'){($sx,$sy,$sd)=($x,$y,0)}
        elsif($c eq '>'){($sx,$sy,$sd)=($x,$y,1)}
        elsif($c eq 'v'){($sx,$sy,$sd)=($x,$y,2)}
        elsif($c eq '<'){($sx,$sy,$sd)=($x,$y,3)}
    }
}
$grid[$sy][$sx] = '.';
my $count = 0;
for my $y (0..$h-1) {
    for my $x (0..$w-1) {
        next if $x==$sx && $y==$sy;
        next unless $grid[$y][$x] eq '.';
        $grid[$y][$x] = '#';
        $count++ if loops(\@grid,$w,$h,$sx,$sy,$sd);
        $grid[$y][$x] = '.';
    }
}
print "$count\n";
sub loops {
    my ($grid,$w,$h,$x,$y,$d) = @_;
    my @dir = ([0,-1],[1,0],[0,1],[-1,0]);
    my %seen;
    for(my $step=0;$step<2000000;$step++){
        my $key = "$x,$y,$d";
        return 1 if $seen{$key}++;
        my ($dx,$dy) = @{$dir[$d]};
        my ($nx,$ny) = ($x+$dx,$y+$dy);
        return 0 if $nx<0 || $ny<0 || $nx>=$w || $ny>=$h;
        if($grid->[$ny][$nx] eq '#'){
            $d = ($d+1)%4;
            next;
        }
        ($x,$y) = ($nx,$ny);
    }
    return 0;
}