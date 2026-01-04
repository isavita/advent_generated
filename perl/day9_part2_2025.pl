
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min max);
use POSIX qw(floor);
open my $fh, '<', 'input.txt' or die $!;
my @points;
my (%ux,%uy);
while (<$fh>) {
    chomp;
    next if /^\s*$/;
    my ($x,$y)=split /,/;
    push @points, [$x+0,$y+0];
    $ux{$x}=1;
    $uy{$y}=1;
}
close $fh;
if (!@points) { print "No points found.\n"; exit; }
my @uniqX = sort {$a<=>$b} keys %ux;
my @uniqY = sort {$a<=>$b} keys %uy;
my %xMap; @xMap{@uniqX}=0..$#uniqX;
my %yMap; @yMap{@uniqY}=0..$#uniqY;
my $gridW = 2*@uniqX+1;
my $gridH = 2*@uniqY+1;
my @colW = (0) x $gridW;
my @rowH = (0) x $gridH;
$colW[0]=1;
for my $i (0..$#uniqX) {
    $colW[2*$i+1]=1;
    if ($i<$#uniqX) {
        my $gap=$uniqX[$i+1]-$uniqX[$i]-1;
        $gap=0 if $gap<0;
        $colW[2*$i+2]=$gap;
    } else { $colW[2*$i+2]=1 }
}
$rowH[0]=1;
for my $i (0..$#uniqY) {
    $rowH[2*$i+1]=1;
    if ($i<$#uniqY) {
        my $gap=$uniqY[$i+1]-$uniqY[$i]-1;
        $gap=0 if $gap<0;
        $rowH[2*$i+2]=$gap;
    } else { $rowH[2*$i+2]=1 }
}
my @grid;
for my $y (0..$gridH-1) { $grid[$y]=[(0) x $gridW] }
sub to_grid {
    my ($p)=@_;
    my $gx=2*$xMap{$p->[0]}+1;
    my $gy=2*$yMap{$p->[1]}+1;
    return ($gx,$gy);
}
my $cnt=@points;
for my $i (0..$cnt-1) {
    my $p1=$points[$i];
    my $p2=$points[($i+1)%$cnt];
    my ($gx1,$gy1)=to_grid($p1);
    my ($gx2,$gy2)=to_grid($p2);
    if ($gx1==$gx2) {
        my ($s,$e)=($gy1,$gy2);
        ($s,$e)=($e,$s) if $s>$e;
        for my $y($s..$e){ $grid[$y][$gx1]=1 if $rowH[$y]>0 }
    } else {
        my ($s,$e)=($gx1,$gx2);
        ($s,$e)=($e,$s) if $s>$e;
        for my $x($s..$e){ $grid[$gy1][$x]=1 if $colW[$x]>0 }
    }
}
my @queue=([0,0]);
$grid[0][0]=2;
my $qidx=0;
my @dirs=([0,1],[0,-1],[1,0],[-1,0]);
while ($qidx<@queue){
    my ($cx,$cy)=@{$queue[$qidx++]};
    for my $d(@dirs){
        my ($nx,$ny)=($cx+$d->[0],$cy+$d->[1]);
        next if $nx<0||$ny<0||$nx>=$gridW||$ny>=$gridH;
        if ($grid[$ny][$nx]==0){
            $grid[$ny][$nx]=2;
            push @queue,[$nx,$ny];
        }
    }
}
my @pref;
for my $y(0..$gridH-1){
    $pref[$y]=[(0) x $gridW];
    for my $x(0..$gridW-1){
        my $val = $grid[$y][$x]==2 ? 0 : $colW[$x]*$rowH[$y];
        my $left  = $x? $pref[$y][$x-1] : 0;
        my $up    = $y? $pref[$y-1][$x] : 0;
        my $diag  = ($x&&$y)? $pref[$y-1][$x-1] : 0;
        $pref[$y][$x] = $val + $left + $up - $diag;
    }
}
sub get_sum{
    my ($x1,$y1,$x2,$y2)=@_;
    ($x1,$x2)=($x2,$x1) if $x1>$x2;
    ($y1,$y2)=($y2,$y1) if $y1>$y2;
    my $total=$pref[$y2][$x2];
    my $left = $x1? $pref[$y2][$x1-1]:0;
    my $up   = $y1? $pref[$y1-1][$x2]:0;
    my $diag = ($x1&&$y1)? $pref[$y1-1][$x1-1]:0;
    return $total-$left-$up+$diag;
}
sub abs{ $_[0] < 0 ? -$_[0] : $_[0] }
my $maxArea=0;
for my $i (0..$#points){
    for my $j ($i..$#points){
        my $p1=$points[$i];
        my $p2=$points[$j];
        my $area = (abs($p1->[0]-$p2->[0])+1)*(abs($p1->[1]-$p2->[1])+1);
        next if $area <= $maxArea;
        my ($gx1,$gy1)=to_grid($p1);
        my ($gx2,$gy2)=to_grid($p2);
        my $valid = get_sum($gx1,$gy1,$gx2,$gy2);
        $maxArea=$area if $valid==$area;
    }
}
print "Largest valid area: $maxArea\n";
