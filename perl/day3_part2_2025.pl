
#!/usr/bin/perl
use strict;
use warnings;
use Math::BigInt lib => 'GMP';

my $k = 12;
my $total = Math::BigInt->new(0);

open my $fh, '<', 'input.txt' or die $!;
while (my $line = <$fh>) {
    chomp $line;
    $line =~ s/^\s+|\s+$//g;
    next if $line eq '';
    next if length $line < $k;

    my $n = length $line;
    my $to_remove = $n - $k;
    my @stack;

    foreach my $c (split //, $line) {
        while ($to_remove && @stack && $stack[-1] lt $c) {
            pop @stack;
            $to_remove--;
        }
        push @stack, $c;
    }
    my $best = join '', @stack[0 .. $k-1];
    $total->badd($best);
}
close $fh;

print "Total output joltage: $total\n";
