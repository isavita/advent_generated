
use strict;
use warnings;
use List::Util qw(sum min);

sub solve {
    my ($buttons, $targets) = @_;
    my $n = @$targets;
    my $numButtons = @$buttons;
    my @matrix;
    for my $j (0..$n-1) {
        $matrix[$j] = [(0) x ($numButtons+1)];
        $matrix[$j][$numButtons] = $targets->[$j];
    }
    for my $i (0..$numButtons-1) {
        for my $j (@{$buttons->[$i]}) {
            $matrix[$j][$i] = 1 if $j < $n;
        }
    }
    my @pivotCol = (-1) x $n;
    my $r = 0;
    for my $c (0..$numButtons-1) {
        last if $r >= $n;
        my $mr = $r;
        for my $i ($r+1..$n-1) {
            $mr = $i if abs($matrix[$i][$c]) > abs($matrix[$mr][$c]);
        }
        next if abs($matrix[$mr][$c]) < 1e-9;
        @matrix[$r, $mr] = @matrix[$mr, $r];
        my $s = $matrix[$r][$c];
        for my $k ($c..$numButtons) {
            $matrix[$r][$k] /= $s;
        }
        for my $i (0..$n-1) {
            next if $i == $r || abs($matrix[$i][$c]) < 1e-9;
            my $f = $matrix[$i][$c];
            for my $k ($c..$numButtons) {
                $matrix[$i][$k] -= $f * $matrix[$r][$k];
            }
        }
        $pivotCol[$r++] = $c;
    }
    my $rank = $r;
    for my $i ($rank..$n-1) {
        return -1 if abs($matrix[$i][$numButtons]) > 1e-9;
    }
    my @isP = (0) x $numButtons;
    for my $i (0..$rank-1) {
        $isP[$pivotCol[$i]] = 1 if $pivotCol[$i] >= 0;
    }
    my @freeVars = grep { !$isP[$_] } 0..$numButtons-1;
    my @maxPresses;
    for my $i (0..$numButtons-1) {
        my $m = 'inf';
        for my $j (@{$buttons->[$i]}) {
            $m = min($m, $targets->[$j]) if $j < $n;
        }
        $maxPresses[$i] = ($m eq 'inf') ? 0 : $m;
    }
    @freeVars = sort { $maxPresses[$a] <=> $maxPresses[$b] } @freeVars;
    my $bestResult = 'inf';
    my @freeValues;
    my $enumerate;
    $enumerate = sub {
        my ($idx, $sum) = @_;
        return if $sum >= $bestResult;
        if ($idx == @freeVars) {
            my @res = (0) x $numButtons;
            for my $i (0..$#freeVars) {
                $res[$freeVars[$i]] = $freeValues[$i];
            }
            for my $i (reverse 0..$rank-1) {
                my $c = $pivotCol[$i];
                next if $c < 0;
                my $v = $matrix[$i][$numButtons];
                for my $k ($c+1..$numButtons-1) {
                    $v -= $matrix[$i][$k] * $res[$k];
                }
                my $iv = int($v + 0.5);
                return if abs($v - $iv) > 1e-6 || $iv < 0 || $iv > $maxPresses[$c];
                $res[$c] = $iv;
            }
            my $cur = sum(@res);
            $bestResult = $cur if $cur < $bestResult;
        } else {
            my $fv = $freeVars[$idx];
            for my $v (0..$maxPresses[$fv]) {
                $freeValues[$idx] = $v;
                $enumerate->($idx+1, $sum+$v);
            }
        }
    };
    $enumerate->(0, 0);
    return $bestResult eq 'inf' ? -1 : $bestResult;
}

open my $fh, '<', 'input.txt' or die $!;
my $total = 0;
while (my $line = <$fh>) {
    chomp $line;
    $line =~ s/^\s+|\s+$//g;
    next if $line eq '';
    my @buttons;
    while ($line =~ /\(([^)]*)\)/g) {
        my $s = $1;
        $s =~ s/^\s+|\s+$//g;
        if ($s eq '') {
            push @buttons, [];
            next;
        }
        my @pts = split /\s*,\s*/, $s;
        push @buttons, \@pts;
    }
    if ($line =~ /\{([^}]*)\}/) {
        my @targets = split /\s*,\s*/, $1;
        my $res = solve(\@buttons, \@targets);
        $total += $res if $res >= 0;
    }
}
close $fh;
print "$total\n";
