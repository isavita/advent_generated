
#!/usr/bin/perl
use strict;
use warnings;

my $filename = 'input.txt';
open my $fh, '<', $filename or die "Cannot open $filename: $!";

my @machines;
while (my $line = <$fh>) {
    chomp $line;
    $line =~ s/^\s+|\s+$//g;
    next unless $line;
    next unless $line =~ /^\[([.#]+)\]/;
    my $diag = $1;
    my @target = map { $_ eq '#' ? 1 : 0 } split //, $diag;
    my @buttons;
    while ($line =~ /\(([\d,]+)\)/g) {
        my @idx = map { 0+$_ } split /,/, $1;
        push @buttons, \@idx;
    }
    push @machines, { target => \@target, buttons => \@buttons };
}
close $fh;

sub solve_machine {
    my ($m) = @_;
    my $R = @{$m->{target}};
    my $C = @{$m->{buttons}};
    my @mat;
    for my $r (0..$R-1) {
        my @row = (0) x ($C+1);
        $row[$C] = $m->{target}[$r];
        for my $c (0..$C-1) {
            for my $idx (@{$m->{buttons}[$c]}) {
                $row[$c] = 1 if $idx == $r;
            }
        }
        $mat[$r] = \@row;
    }
    return gaussian_min_weight(\@mat, $R, $C);
}

sub gaussian_min_weight {
    my ($mat_ref, $R, $C) = @_;
    my @mat = @$mat_ref;
    my $pivotRow = 0;
    my @colIsPivot = (0) x $C;

    for my $c (0..$C-1) {
        last if $pivotRow >= $R;
        my $sel = -1;
        for my $r ($pivotRow..$R-1) {
            if ($mat[$r][$c]) { $sel = $r; last }
        }
        next if $sel == -1;
        @mat[$pivotRow,$sel] = @mat[$sel,$pivotRow];
        for my $r (0..$R-1) {
            next if $r == $pivotRow;
            if ($mat[$r][$c]) {
                for my $k ($c..$C) { $mat[$r][$k] ^= $mat[$pivotRow][$k] }
            }
        }
        $colIsPivot[$c] = 1;
        $pivotRow++;
    }

    for my $r ($pivotRow..$R-1) {
        return -1 if $mat[$r][$C];
    }

    my @freeVars = grep { !$colIsPivot[$_] } 0..$C-1;
    my $numFree = @freeVars;
    my $limit = 1 << $numFree;
    my $minWeight = $R*$C+1;

    for my $mask (0..$limit-1) {
        my @x = (0) x $C;
        my $weight = 0;
        for my $j (0..$numFree-1) {
            if ($mask >> $j & 1) {
                $x[$freeVars[$j]] = 1;
                $weight++;
            }
        }
        my $currPivotRow = 0;
        for my $c (0..$C-1) {
            if ($colIsPivot[$c]) {
                my $val = $mat[$currPivotRow][$C];
                for my $k ($c+1..$C-1) {
                    $val ^= $x[$k] if $mat[$currPivotRow][$k];
                }
                $x[$c] = $val;
                $weight += $val;
                $currPivotRow++;
            }
        }
        $minWeight = $weight if $weight < $minWeight;
    }
    return $minWeight;
}

my $total = 0;
for my $m (@machines) {
    my $mp = solve_machine($m);
    warn "No solution for a machine\n" if $mp == -1;
    $total += $mp if $mp != -1;
}
print "Fewest total button presses: $total\n";
