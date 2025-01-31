
use strict;
use warnings;
use List::Util qw(min);

my @grid;
open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @grid, $line;
}
close $fh;

my ($n, $m) = (scalar @grid, length $grid[0]);
my ($sx, $sy, $ex, $ey);
for my $i (0 .. $n - 1) {
    for my $j (0 .. $m - 1) {
        if (substr($grid[$i], $j, 1) eq 'S') {
            ($sx, $sy) = ($i, $j);
        } elsif (substr($grid[$i], $j, 1) eq 'E') {
            ($ex, $ey) = ($i, $j);
        }
    }
}

my @dx = (-1, 0, 1, 0);
my @dy = (0, 1, 0, -1);

my @dist;
for my $i (0 .. $n - 1) {
    $dist[$i] = [];
    for my $j (0 .. $m - 1) {
        $dist[$i][$j] = [ (1e9) x 4 ];
    }
}
$dist[$sx][$sy][1] = 0;

my @h = ( { x => $sx, y => $sy, d => 1, cost => 0 } );

sub push_heap {
    my ($h, $v) = @_;
    push @$h, $v;
    my $i = scalar @$h - 1;
    while ($i > 0) {
        my $p = ($i - 1) >> 1;
        last if $h->[$p]{cost} <= $h->[$i]{cost};
        @$h[$p, $i] = @$h[$i, $p];
        $i = $p;
    }
}

sub pop_heap {
    my ($h) = @_;
    my $v = shift @$h;
    if (@$h) {
        my $last = pop @$h;
        unshift @$h, $last;
        my $i = 0;
        while (1) {
            my $l = 2 * $i + 1;
            my $r = 2 * $i + 2;
            my $small = $i;
            $small = $l if $l < @$h && $h->[$l]{cost} < $h->[$small]{cost};
            $small = $r if $r < @$h && $h->[$r]{cost} < $h->[$small]{cost};
            last if $small == $i;
            @$h[$i, $small] = @$h[$small, $i];
            $i = $small;
        }
    }
    return $v;
}

while (@h) {
    my $u = pop_heap(\@h);
    next if $dist[$u->{x}][$u->{y}][$u->{d}] < $u->{cost};
    if ($u->{x} == $ex && $u->{y} == $ey) {
        print $u->{cost}, "\n";
        exit;
    }
    for my $ndir (($u->{d} + 1) % 4, ($u->{d} + 3) % 4) {
        my $nc = $u->{cost} + 1000;
        if ($nc < $dist[$u->{x}][$u->{y}][$ndir]) {
            $dist[$u->{x}][$u->{y}][$ndir] = $nc;
            push_heap(\@h, { x => $u->{x}, y => $u->{y}, d => $ndir, cost => $nc });
        }
    }
    my $nx = $u->{x} + $dx[$u->{d}];
    my $ny = $u->{y} + $dy[$u->{d}];
    if ($nx >= 0 && $nx < $n && $ny >= 0 && $ny < $m && substr($grid[$nx], $ny, 1) ne '#') {
        my $nc = $u->{cost} + 1;
        if ($nc < $dist[$nx][$ny][$u->{d}]) {
            $dist[$nx][$ny][$u->{d}] = $nc;
            push_heap(\@h, { x => $nx, y => $ny, d => $u->{d}, cost => $nc });
        }
    }
}
