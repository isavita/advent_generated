#!/usr/bin/perl
use strict;
use warnings;

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @instructions = <$fh>;
close $fh;

my %registers;
my $mul_count = 0;
my $pointer = 0;

sub get_value {
    my ($v) = @_;
    return $v =~ /^-?\d+$/ ? $v : $registers{$v} // 0;
}

while ($pointer >= 0 && $pointer < @instructions) {
    my ($cmd, $x, $y) = split ' ', $instructions[$pointer];

    if ($cmd eq 'set') {
        $registers{$x} = get_value($y);
    } elsif ($cmd eq 'sub') {
        $registers{$x} -= get_value($y);
    } elsif ($cmd eq 'mul') {
        $registers{$x} *= get_value($y);
        $mul_count++;
    } elsif ($cmd eq 'jnz') {
        if (get_value($x) != 0) {
            $pointer += get_value($y) - 1;
        }
    }
    $pointer++;
}

print $mul_count, "\n";