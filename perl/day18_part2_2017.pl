#!/usr/bin/perl
use strict;
use warnings;

sub get_value {
    my ($arg, $registers) = @_;
    return $arg =~ /^-?\d+$/ ? $arg : $registers->{$arg};
}

open my $fh, '<', 'input.txt' or die "Cannot open input.txt: $!";
my @instructions = map { [split] } <$fh>;
close $fh;

my %registers0 = ('p' => 0);
my %registers1 = ('p' => 1);
my @queue0;
my @queue1;
my $send_count1 = 0;
my ($i0, $i1) = (0, 0);
my ($deadlock0, $deadlock1) = (0, 0);

while (!($deadlock0 && $deadlock1)) {
    $deadlock0 = $deadlock1 = 1;

    # Program 0
    while ($i0 < @instructions) {
        my ($cmd, $arg1, $arg2) = @{$instructions[$i0]};
        if ($cmd eq 'snd') {
            push @queue1, get_value($arg1, \%registers0);
        } elsif ($cmd eq 'set') {
            $registers0{$arg1} = get_value($arg2, \%registers0);
        } elsif ($cmd eq 'add') {
            $registers0{$arg1} += get_value($arg2, \%registers0);
        } elsif ($cmd eq 'mul') {
            $registers0{$arg1} *= get_value($arg2, \%registers0);
        } elsif ($cmd eq 'mod') {
            $registers0{$arg1} %= get_value($arg2, \%registers0);
        } elsif ($cmd eq 'rcv') {
            last unless @queue0;
            $registers0{$arg1} = shift @queue0;
        } elsif ($cmd eq 'jgz') {
            if (get_value($arg1, \%registers0) > 0) {
                $i0 += get_value($arg2, \%registers0);
                next;
            }
        }
        $i0++;
        $deadlock0 = 0;
    }

    # Program 1
    while ($i1 < @instructions) {
        my ($cmd, $arg1, $arg2) = @{$instructions[$i1]};
        if ($cmd eq 'snd') {
            push @queue0, get_value($arg1, \%registers1);
            $send_count1++;
        } elsif ($cmd eq 'set') {
            $registers1{$arg1} = get_value($arg2, \%registers1);
        } elsif ($cmd eq 'add') {
            $registers1{$arg1} += get_value($arg2, \%registers1);
        } elsif ($cmd eq 'mul') {
            $registers1{$arg1} *= get_value($arg2, \%registers1);
        } elsif ($cmd eq 'mod') {
            $registers1{$arg1} %= get_value($arg2, \%registers1);
        } elsif ($cmd eq 'rcv') {
            last unless @queue1;
            $registers1{$arg1} = shift @queue1;
        } elsif ($cmd eq 'jgz') {
            if (get_value($arg1, \%registers1) > 0) {
                $i1 += get_value($arg2, \%registers1);
                next;
            }
        }
        $i1++;
        $deadlock1 = 0;
    }
}

print "$send_count1\n";