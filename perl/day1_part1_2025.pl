
#!/usr/bin/perl
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die $!;
my $pos = 50;
my $zero = 0;
while (my $line = <$fh>) {
    chomp $line;
    next unless $line =~ /\S/;
    my ($dir, $amt) = $line =~ /^([LR])(\d+)$/;
    $amt = 0 + $amt;
    $pos = ($dir eq 'R')
        ? ($pos + $amt) % 100
        : ($pos - $amt) % 100;
    $pos += 100 if $pos < 0;
    $zero++ if $pos == 0;
}
close $fh;
print "The password is: $zero\n";
