
use strict;
use warnings;

my $mod = 1 << 24;
my $num_steps = 2000;

sub next_secret {
    my $s = shift;
    my $x = $s * 64;
    $s ^= $x;
    $s &= $mod - 1;
    $x = int($s / 32);
    $s ^= $x;
    $s &= $mod - 1;
    $x = $s * 2048;
    $s ^= $x;
    $s &= $mod - 1;
    return $s;
}

sub encode_change4 {
    my ($c1, $c2, $c3, $c4) = @_;
    return ($c1 + 9) + ($c2 + 9) * 19 + ($c3 + 9) * 19 * 19 + ($c4 + 9) * 19 * 19 * 19;
}

open(my $fh, '<', 'input.txt') or die "Error opening input file: $!";
my @initials;
while (my $line = <$fh>) {
    chomp $line;
    next if $line eq '';
    push @initials, $line;
}
close $fh;

my @buyers;
for my $init_val (@initials) {
    my @prices;
    my $s = $init_val;
    for (my $j = 0; $j <= $num_steps; $j++) {
        push @prices, $s % 10;
        if ($j < $num_steps) {
            $s = next_secret($s);
        }
    }
    my @changes;
    for (my $j = 0; $j < $num_steps; $j++) {
        push @changes, $prices[$j + 1] - $prices[$j];
    }
    push @buyers, { prices => \@prices, changes => \@changes };
}

my $pattern_count = 19 * 19 * 19 * 19;
my @global_sum = (0) x $pattern_count;

for my $b (@buyers) {
    my @local_price = (-1) x $pattern_count;
    for (my $i = 0; $i + 3 < $num_steps; $i++) {
        my ($c1, $c2, $c3, $c4) = (
            $b->{changes}->[$i], $b->{changes}->[$i + 1],
            $b->{changes}->[$i + 2], $b->{changes}->[$i + 3]
        );
        next if $c1 < -9 || $c1 > 9 || $c2 < -9 || $c2 > 9 ||
                $c3 < -9 || $c3 > 9 || $c4 < -9 || $c4 > 9;
        my $idx = encode_change4($c1, $c2, $c3, $c4);
        if ($local_price[$idx] < 0) {
            $local_price[$idx] = $b->{prices}->[$i + 4];
        }
    }
    for (my $idx = 0; $idx < $pattern_count; $idx++) {
        if ($local_price[$idx] >= 0) {
            $global_sum[$idx] += $local_price[$idx];
        }
    }
}

my $ans = 0;
for my $s (@global_sum) {
    $ans = $s if $s > $ans;
}

print "$ans\n";
