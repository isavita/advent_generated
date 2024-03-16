use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;
my @nums;
my $i = 0;
while (my $line = <$fh>) {
    chomp $line;
    push @nums, {pos => $i, val => int($line)};
    $i++;
}
close($fh);

my @nums2 = map {{pos => $_->{pos}, val => 811589153 * $_->{val}}} @nums;

for (my $i = 0; $i < 10; $i++) {
    mix(\@nums2);
}
print coords(\@nums2) . "\n";

sub mix {
    my ($nums) = @_;
    my $n = scalar(@$nums) - 1;
    for my $i (0..$#$nums) {
        my $oldpos = $nums->[$i]{pos};
        my $newpos = (($oldpos + $nums->[$i]{val}) % $n + $n) % $n;
        if ($oldpos < $newpos) {
            for my $j (0..$#$nums) {
                if ($nums->[$j]{pos} > $oldpos && $nums->[$j]{pos} <= $newpos) {
                    $nums->[$j]{pos}--;
                }
            }
        }
        if ($newpos < $oldpos) {
            for my $j (0..$#$nums) {
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
    my $l = scalar(@$nums);
    my $zeroPos;
    for my $i (0..$#$nums) {
        if ($nums->[$i]{val} == 0) {
            $zeroPos = $nums->[$i]{pos};
            last;
        }
    }
    my $sum = 0;
    for my $i (0..$#$nums) {
        if ($nums->[$i]{pos} == ($zeroPos+1000)%$l || $nums->[$i]{pos} == ($zeroPos+2000)%$l || $nums->[$i]{pos} == ($zeroPos+3000)%$l) {
            $sum += $nums->[$i]{val};
        }
    }
    return $sum;
}