
#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(sum);

sub trim_leading_zeros {
    my $s = shift;
    $s =~ s/^0+(?=\d)//;
    return $s eq '' ? '0' : $s;
}

sub split_stone {
    my $s = shift;
    my $mid = length($s) / 2;
    my $left = trim_leading_zeros(substr($s, 0, $mid));
    my $right = trim_leading_zeros(substr($s, $mid));
    return ($left, $right);
}

sub multiply_by_2024 {
    my $s = shift;
    my @num = split //, $s;
    my @multiplier = (2, 0, 2, 4);
    my @result = (0) x (length($s) + 4);

    for (my $i = length($s) - 1; $i >= 0; $i--) {
        my $carry = 0;
        for (my $j = 3; $j >= 0; $j--) {
            my $product = ($num[$i] * $multiplier[$j]) + $result[$i + $j + 1] + $carry;
            $result[$i + $j + 1] = $product % 10;
            $carry = int($product / 10);
        }
        $result[$i] += $carry;
    }

    my $start = 0;
    $start++ while $start < scalar(@result) - 1 && $result[$start] == 0;
    return join '', @result[$start..$#result];
}

open my $fh, '<', "input.txt" or die "Could not open input.txt: $!";
my $line = <$fh>;
chomp $line;
close $fh;

my %stones;
$stones{$_}++ for split ' ', $line;

for (1..75) {
    my %new_stones;
    while (my ($stone, $count) = each %stones) {
        if ($stone eq '0') {
            $new_stones{'1'} += $count;
        } elsif (length($stone) % 2 == 0) {
            my ($left, $right) = split_stone($stone);
            $new_stones{$left} += $count;
            $new_stones{$right} += $count;
        } else {
            my $new_stone = multiply_by_2024($stone);
            $new_stones{$new_stone} += $count;
        }
    }
    %stones = %new_stones;
}

my $total_stones = sum values %stones;
print "$total_stones\n";
