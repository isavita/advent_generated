
use strict;
use warnings;

sub can_produce {
  my ($target, $nums, $idx, $current) = @_;
  return $current == $target if $idx == @$nums;
  return can_produce($target, $nums, $idx + 1, $current + $nums->[$idx]) ||
         can_produce($target, $nums, $idx + 1, $current * $nums->[$idx]);
}

my $total = 0;
open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
while (my $line = <$fh>) {
  chomp $line;
  next if $line eq '';
  my ($target, $nums_str) = split /:/, $line;
  $target = int $target;
  my @nums = map { int $_ } split /\s+/, $nums_str;
  if (@nums == 1) {
    $total += $target if $nums[0] == $target;
    next;
  }
  $total += $target if can_produce($target, \@nums, 1, $nums[0]);
}
close $fh;
print $total, "\n";
