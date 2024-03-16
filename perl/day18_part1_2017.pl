#!/usr/bin/perl

use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @instructions;
while (my $line = <$fh>) {
    chomp $line;
    push @instructions, [split ' ', $line];
}
close($fh);

my %registers;
my $lastSound;

my $i = 0;
while ($i < scalar @instructions) {
    my $instruction = $instructions[$i];
    my $cmd = $instruction->[0];
    my $arg1 = $instruction->[1];

    if ($cmd eq "snd") {
        $lastSound = getValue($arg1, \%registers);
    } elsif ($cmd eq "set") {
        $registers{$arg1} = getValue($instruction->[2], \%registers);
    } elsif ($cmd eq "add") {
        $registers{$arg1} += getValue($instruction->[2], \%registers);
    } elsif ($cmd eq "mul") {
        $registers{$arg1} *= getValue($instruction->[2], \%registers);
    } elsif ($cmd eq "mod") {
        $registers{$arg1} %= getValue($instruction->[2], \%registers);
    } elsif ($cmd eq "rcv") {
        if (getValue($arg1, \%registers) != 0) {
            print "$lastSound\n";
            exit;
        }
    } elsif ($cmd eq "jgz") {
        if (getValue($arg1, \%registers) > 0) {
            $i += getValue($instruction->[2], \%registers);
            next;
        }
    }
    $i++;
}

sub getValue {
    my ($arg, $registers) = @_;
    if ($arg =~ /^\d+$/ || $arg =~ /^-\d+$/) {
        return $arg;
    } else {
        return $registers->{$arg} // 0;
    }
}