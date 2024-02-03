
use strict;
use warnings;

my $sum = 0;
open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    $sum += fromSnafu($line);
}
close($fh);
print toSnafu($sum) . "\n";

sub fromSnafu {
    my ($s) = @_;
    my $n = 0;
    foreach my $char (split //, $s) {
        $n *= 5;
        if ($char eq '=') {
            $n -= 2;
        } elsif ($char eq '-') {
            $n--;
        } else {
            $n += ord($char) - ord('0');
        }
    }
    return $n;
}

sub toSnafu {
    my ($n) = @_;
    my @b;
    while ($n > 0) {
        if ($n % 5 == 3) {
            $n += 5;
            push @b, '=';
        } elsif ($n % 5 == 4) {
            $n += 5;
            push @b, '-';
        } else {
            push @b, chr(ord('0') + $n % 5);
        }
        $n /= 5;
    }
    for (my $i = 0; $i < scalar @b / 2; $i++) {
        ($b[$i], $b[$#b - $i]) = ($b[$#b - $i], $b[$i]);
    }
    return join '', @b;
}
