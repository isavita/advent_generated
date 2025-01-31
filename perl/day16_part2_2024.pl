
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Can't open input.txt: $!";
my @grid = map { chomp; $_ } <$fh>;
close($fh);

my ($n, $m) = (scalar @grid, length $grid[0]);
my ($sx, $sy, $ex, $ey);
for my $i (0 .. $n - 1) {
    for my $j (0 .. $m - 1) {
        if ($grid[$i] =~ /S/ && $-[0] == $j) {
            ($sx, $sy) = ($i, $j);
        } elsif ($grid[$i] =~ /E/ && $-[0] == $j) {
            ($ex, $ey) = ($i, $j);
        }
    }
}

my @dx = (-1, 0, 1, 0);
my @dy = (0, 1, 0, -1);

my @dist;
for my $i (0 .. $n - 1) {
    for my $j (0 .. $m - 1) {
        push @{$dist[$i][$j]}, (1e9, 1e9, 1e9, 1e9);
    }
}
$dist[$sx][$sy][1] = 0;

my @h = ([$sx, $sy, 1, 0]);

while (@h) {
    my $u = shift @h;
    my ($x, $y, $d, $cost) = @$u;
    if ($dist[$x][$y][$d] < $cost) {
        next;
    }
    if ($x == $ex && $y == $ey) {
        next;
    }

    for my $ndir (($d + 1) % 4, ($d + 3) % 4) {
        my $nc = $cost + 1000;
        if ($nc < $dist[$x][$y][$ndir]) {
            $dist[$x][$y][$ndir] = $nc;
            push @h, [$x, $y, $ndir, $nc];
            @h = sort { $a->[3] <=> $b->[3] } @h;
        }
    }
    my ($nx, $ny) = ($x + $dx[$d], $y + $dy[$d]);
    if ($nx >= 0 && $nx < $n && $ny >= 0 && $ny < $m && substr($grid[$nx], $ny, 1) ne '#') {
        my $nc = $cost + 1;
        if ($nc < $dist[$nx][$ny][$d]) {
            $dist[$nx][$ny][$d] = $nc;
            push @h, [$nx, $ny, $d, $nc];
            @h = sort { $a->[3] <=> $b->[3] } @h;
        }
    }
}

my $best = 1e9;
for my $d (0 .. 3) {
    if ($dist[$ex][$ey][$d] < $best) {
        $best = $dist[$ex][$ey][$d];
    }
}

my @used;
for my $i (0 .. $n - 1) {
    push @used, [(0) x $m];
}

my @rev;
for my $d (0 .. 3) {
    if ($dist[$ex][$ey][$d] == $best) {
        push @rev, [$ex, $ey, $d];
    }
}

my @vis;
for my $i (0 .. $n - 1) {
    for my $j (0 .. $m - 1) {
        push @{$vis[$i][$j]}, (0, 0, 0, 0);
    }
}
for my $s (@rev) {
    $vis[$s->[0]][$s->[1]][$s->[2]] = 1;
}

while (@rev) {
    my $u = pop @rev;
    my ($x, $y, $d) = @$u;
    $used[$x][$y] = 1;

    my $costU = $dist[$x][$y][$d];

    for my $pd (($d + 1) % 4, ($d + 3) % 4) {
        if ($dist[$x][$y][$pd] == $costU - 1000) {
            if (!$vis[$x][$y][$pd]) {
                $vis[$x][$y][$pd] = 1;
                push @rev, [$x, $y, $pd];
            }
        }
    }

    my ($px, $py) = ($x - $dx[$d], $y - $dy[$d]);
    if ($px >= 0 && $px < $n && $py >= 0 && $py < $m && substr($grid[$px], $py, 1) ne '#') {
        if ($dist[$px][$py][$d] == $costU - 1) {
            if (!$vis[$px][$py][$d]) {
                $vis[$px][$py][$d] = 1;
                push @rev, [$px, $py, $d];
            }
        }
    }
}

my $cnt = 0;
for my $i (0 .. $n - 1) {
    for my $j (0 .. $m - 1) {
        if ($used[$i][$j] && substr($grid[$i], $j, 1) ne '#') {
            $cnt++;
        }
    }
}

print "$cnt\n";
