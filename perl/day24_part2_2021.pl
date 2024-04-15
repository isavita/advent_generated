use strict;
use warnings;

my @k;
my @l;
my @m;

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @lines = <$fh>;
close $fh;

for (my $i = 0; $i < @lines; $i++) {
    my $line = $lines[$i];
    chomp $line;
    
    if ($i % 18 == 4) {
        my ($v) = ($line =~ /div z (\d+)/);
        push @l, $v;
    } elsif ($i % 18 == 5) {
        my ($v) = ($line =~ /add x (-?\d+)/);
        push @k, $v;
    } elsif ($i % 18 == 15) {
        my ($v) = ($line =~ /add y (-?\d+)/);
        push @m, $v;
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

my @min = (0) x 14;
for my $i (0 .. 13) {
    next unless exists $constraints{$i};
    
    my $vmin = 1;
    while ($vmin + $constraints{$i}[1] < 1) {
        $vmin++;
    }
    $min[$i] = $vmin;
    $min[$constraints{$i}[0]] = $vmin + $constraints{$i}[1];
}

print num(@min), "\n";

sub num {
    my @w = @_;
    my $n = 0;
    for my $i (0 .. $#w) {
        $n *= 10;
        $n += $w[$i];
    }
    return $n;
}