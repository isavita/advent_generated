#!/usr/bin/perl
use strict;
use warnings;
use integer;
use File::Slurp;

my $filename = "input.txt";
my $input = read_file($filename);
$input =~ /depth: (\d+)\s+target: (\d+),(\d+)/ or die;
my ($depth, $target_x, $target_y) = ($1, $2, $3);

my $ToolNone  = 1;
my $ToolTorch = 2;
my $ToolGear  = 4;
my $TypeRocky  = 0;
my $TypeWet    = 1;
my $TypeNarrow = 2;
my $geoMultX = 16807;
my $geoMultY = 48271;
my $caveModulo = 20183;
my $bailFactor = 8;

my (%geo, %erosion);

sub geo_index {
    my ($x, $y) = @_;
    my $key = "$x,$y";
    return $geo{$key} if exists $geo{$key};
    my $val;
    if (($x == 0 && $y == 0) || ($x == $target_x && $y == $target_y)) {
        $val = 0;
    } elsif ($y == 0) {
        $val = $x * $geoMultX;
    } elsif ($x == 0) {
        $val = $y * $geoMultY;
    } else {
        $val = erosion($x-1, $y) * erosion($x, $y-1);
    }
    $geo{$key} = $val;
    return $val;
}

sub erosion {
    my ($x, $y) = @_;
    my $key = "$x,$y";
    return $erosion{$key} if exists $erosion{$key};
    my $e = (geo_index($x,$y) + $depth) % $caveModulo;
    $erosion{$key} = $e;
    return $e;
}

sub region_type {
    my ($x, $y) = @_;
    return erosion($x,$y) % 3;
}

sub allowed {
    my ($rtype) = @_;
    return $rtype== $TypeRocky  ? ($ToolGear | $ToolTorch)
         : $rtype== $TypeWet    ? ($ToolGear | $ToolNone)
         : $rtype== $TypeNarrow ? ($ToolTorch | $ToolNone)
         : die "unknown type $rtype";
}

sub neighbors {
    my ($x, $y) = @_;
    my @n;
    push @n, [$x+1, $y], [$x, $y+1];
    push @n, [$x-1, $y] if $x > 0;
    push @n, [$x, $y-1] if $y > 0;
    return @n;
}

# Binary heap (min-heap by time) implementation.
sub heap_push {
    my ($heap, $item) = @_;
    push @$heap, $item;
    my $i = $#$heap;
    while ($i > 0) {
        my $p = int(($i - 1)/2);
        if ($heap->[$i][0] < $heap->[$p][0]) {
            @$heap[$i, $p] = @$heap[$p, $i];
            $i = $p;
        } else {
            last;
        }
    }
}
sub heap_pop {
    my ($heap) = @_;
    return undef unless @$heap;
    my $min = $heap->[0];
    my $last = pop @$heap;
    if (@$heap) {
        $heap->[0] = $last;
        my $i = 0;
        my $n = scalar(@$heap);
        while (1) {
            my $l = 2*$i+1;
            my $r = 2*$i+2;
            my $small = $i;
            if ($l < $n && $heap->[$l][0] < $heap->[$small][0]) {
                $small = $l;
            }
            if ($r < $n && $heap->[$r][0] < $heap->[$small][0]) {
                $small = $r;
            }
            last if $small == $i;
            @$heap[$i, $small] = @$heap[$small, $i];
            $i = $small;
        }
    }
    return $min;
}

my %dist;
my @heap;
my $start_state = "0,0,$ToolTorch";
$dist{$start_state} = 0;
heap_push(\@heap, [0, 0, 0, $ToolTorch]);

while (@heap) {
    my ($time, $x, $y, $equip) = @{ heap_pop(\@heap) };
    if ($x == $target_x && $y == $target_y && $equip == $ToolTorch) {
        print "$time\n";
        exit;
    }
    next if $x > $bailFactor * $target_x || $y > $bailFactor * $target_y;
    my $state_key = "$x,$y,$equip";
    next if exists($dist{$state_key}) && $dist{$state_key} < $time;
    for my $nb (neighbors($x, $y)) {
        my ($nx, $ny) = @$nb;
        my $rtype = region_type($nx, $ny);
        my $allow = allowed($rtype);
        next unless ($equip & $allow);
        for my $transition ( [ $equip, 1 ], [ $equip ^ $allow, 8 ] ) {
            my ($nequip, $cost) = @$transition;
            my $nt = $time + $cost;
            my $nstate = "$nx,$ny,$nequip";
            if (!exists($dist{$nstate}) || $nt < $dist{$nstate}) {
                $dist{$nstate} = $nt;
                heap_push(\@heap, [$nt, $nx, $ny, $nequip]);
            }
        }
    }
}
print "0\n";