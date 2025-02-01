
#!/usr/bin/perl
use strict;
use warnings;

sub parse_lock {
    my @block = @_;
    my @h;
    for my $c (0..4) {
        my $cnt = 0;
        for my $r (1..6) {
            if (substr($block[$r], $c, 1) eq '#') {
                $cnt++;
            } else {
                last;
            }
        }
        push @h, $cnt;
    }
    return \@h;
}

sub parse_key {
    my @block = @_;
    my @h;
    for my $c (0..4) {
        my $cnt = 0;
        for my $r (reverse 0..5) {
            if (substr($block[$r], $c, 1) eq '#') {
                $cnt++;
            } else {
                last;
            }
        }
        push @h, $cnt;
    }
    return \@h;
}

sub fits {
    my ($lock, $key) = @_;
    for my $i (0..4) {
        if ($lock->[$i] + $key->[$i] > 5) {
            return 0;
        }
    }
    return 1;
}

sub all_char {
    my ($s, $ch) = @_;
    return $s =~ /^$ch*$/;
}

my @raw;
open my $fh, '<', "input.txt" or die "Could not open input.txt: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @raw, $line if $line =~ /\S/;
}
close $fh;

if (@raw % 7 != 0) {
    print "0\n";
    exit;
}

my @locks;
my @keys;

for (my $i = 0; $i + 7 <= @raw; $i += 7) {
    my @block = @raw[$i..$i+6];
    my $valid = 1;
    for my $ln (@block) {
      if (length($ln) < 5) {
        $valid = 0;
        last;
      }
    }
    next unless $valid;
    if (all_char($block[0], '#')) {
        push @locks, parse_lock(@block);
    } else {
        push @keys, parse_key(@block);
    }
}

my $count = 0;
for my $lock (@locks) {
    for my $key (@keys) {
        $count++ if fits($lock, $key);
    }
}

print "$count\n";
