
use strict;
use warnings;

my @nums;
open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
while (my $line = <$fh>) {
    chomp $line;
    push @nums, { pos => scalar @nums, val => int($line) };
}
close($fh);

my @nums2;
foreach my $i (0..$#nums) {
    $nums2[$i] = { pos => $nums[$i]{pos}, val => 811589153 * $nums[$i]{val} };
}

mix(\@nums);
print coords(\@nums) . "\n";

sub mix {
    my ($nums) = @_;
    my $n = scalar @$nums - 1;
    foreach my $i (0..$#$nums) {
        my $oldpos = $nums->[$i]{pos};
        my $newpos = (($oldpos + $nums->[$i]{val}) % $n + $n) % $n;
        if ($oldpos < $newpos) {
            foreach my $j (0..$#$nums) {
                if ($nums->[$j]{pos} > $oldpos && $nums->[$j]{pos} <= $newpos) {
                    $nums->[$j]{pos}--;
                }
            }
        } else {
            foreach my $j (0..$#$nums) {
                if ($nums->[$j]{pos} >= $newpos && $nums->[$j]{pos} < $oldpos) {
                    $nums->[$j]{pos}++;
                }
            }
        }
        $nums->[$i]{pos} = $newpos;
    }
}

sub coords {
    my ($nums) = @_;
    my $l = scalar @$nums;
    my $zeroPos;
    foreach my $i (0..$#$nums) {
        if ($nums->[$i]{val} == 0) {
            $zeroPos = $nums->[$i]{pos};
            last;
        }
    }
    my $sum = 0;
    foreach my $i (0..$#$nums) {
        if ($nums->[$i]{pos} == ($zeroPos + 1000) % $l || $nums->[$i]{pos} == ($zeroPos + 2000) % $l || $nums->[$i]{pos} == ($zeroPos + 3000) % $l) {
            $sum += $nums->[$i]{val};
        }
    }
    return $sum;
}
