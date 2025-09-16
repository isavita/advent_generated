#!/usr/bin/perl
use strict;
use warnings;

sub main {
  open my $fh, '<', 'input.txt' or die "input.txt: $!";
  while (<$fh>) { }
  close $fh;

  my %seen;
  my $last_unique = 0;
  my $register5 = 0;

  while (1) {
    my $register3 = $register5 | 65536;
    $register5 = 7586220;

    while (1) {
      my $register1 = $register3 % 256;
      my $tmp = ($register5 + $register1) % 16777216;
      $register5 = ($tmp * 65899) % 16777216;

      if ($register3 < 256) {
        if (exists $seen{$register5}) {
          print "Part Two Answer: $last_unique\n";
          return;
        }
        $seen{$register5} = 1;
        $last_unique = $register5;
        if (scalar(keys %seen) == 1) {
          print "Part One Answer: $register5\n";
        }
        last;
      } else {
        $register3 = int($register3 / 256);
      }
    }
  }
}

main();