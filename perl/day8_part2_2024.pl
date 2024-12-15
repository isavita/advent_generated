
#!/usr/bin/perl
use strict;
use warnings;

sub gcd {
    my ($a, $b) = @_;
    return $a if $b == 0;
    return gcd($b, $a % $b);
}

open my $fh, "<", "input.txt" or die "Could not open input.txt: $!";
my @grid = <$fh>;
close $fh;
chomp @grid;

my $h = @grid;
my $w = length $grid[0];
my %antennas;

for my $y (0..$h-1) {
    for my $x (0..$w-1) {
        my $c = substr($grid[$y], $x, 1);
        if ($c ne '.') {
            push @{$antennas{$c}}, [$y, $x];
        }
    }
}

my %linesPerFreq;
for my $f (keys %antennas) {
    my @coords = @{$antennas{$f}};
    my %lines;
    for (my $i = 0; $i < @coords; $i++) {
        for (my $j = $i + 1; $j < @coords; $j++) {
            my ($ay, $ax) = @{$coords[$i]};
            my ($by, $bx) = @{$coords[$j]};
            my $dy = $by - $ay;
            my $dx = $bx - $ax;
            my $g = gcd($dy, $dx);
            my $sy = $dy / $g;
            my $sx = $dx / $g;
            if ($sx < 0 || ($sx == 0 && $sy < 0)) {
                $sx = -$sx;
                $sy = -$sy;
            }
            my $c = $sy * $ax - $sx * $ay;
            my $key = "$sx,$sy,$c";
            $lines{$key} = 1;
        }
    }
    $linesPerFreq{$f} = \%lines;
}

my %antinodes;
for my $lines_ref (values %linesPerFreq) {
    for my $key (keys %$lines_ref) {
        my ($sx, $sy, $c) = split ',', $key;
        if ($sx == 0 && $sy == 0) {
            next;
        }
        if ($sy == 0) {
            if ($c % $sx == 0) {
                my $y = -$c / $sx;
                if ($y >= 0 && $y < $h) {
                    for (my $x = 0; $x < $w; $x++) {
                        $antinodes{"$y,$x"} = 1;
                    }
                }
            }
        } elsif ($sx == 0) {
            if ($c % $sy == 0) {
                my $x = $c / $sy;
                if ($x >= 0 && $x < $w) {
                    for (my $y = 0; $y < $h; $y++) {
                        $antinodes{"$y,$x"} = 1;
                    }
                }
            }
        } else {
            for (my $y = 0; $y < $h; $y++) {
                my $val = $c + $sx * $y;
                if ($val % $sy == 0) {
                    my $x = $val / $sy;
                    if ($x >= 0 && $x < $w) {
                        $antinodes{"$y,$x"} = 1;
                    }
                }
            }
        }
    }
}

print scalar keys %antinodes, "\n";
