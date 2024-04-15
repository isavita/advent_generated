use strict;
use warnings;

my @k;
my @l;
my @m;

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @lines = <$fh>;
close $fh;

for (my $i = 0; $i < @lines; $i++) {
    if ($i % 18 == 4) {
        if ($lines[$i] =~ /div z (\d+)/) {
            push @l, $1;
        }
    } elsif ($i % 18 == 5) {
        if ($lines[$i] =~ /add x (-?\d+)/) {
            push @k, $1;
        }
    } elsif ($i % 18 == 15) {
        if ($lines[$i] =~ /add y (-?\d+)/) {
            push @m, $1;
        }
    }
}

my %constraints;
my @stack;
for (my $i = 0; $i < @l; $i++) {
    if ($l[$i] == 1) {
        push @stack, $i;
    } elsif ($l[$i] == 26) {
        my $pop = pop @stack;
        $constraints{$pop} = [$i, $m[$pop] + $k[$i]];
    }
}

my @max = (0) x 14;
for my $i (0 .. 13) {
    if (exists $constraints{$i}) {
        my $vmax = 9;
        while ($vmax + $constraints{$i}[1] > 9) {
            $vmax--;
        }
        $max[$i] = $vmax;
        $max[$constraints{$i}[0]] = $vmax + $constraints{$i}[1];
    }
}

print num(@max), "\n";

sub num {
    my $n = 0;
    for my $w (@_) {
        $n *= 10;
        $n += $w;
    }
    return $n;
}