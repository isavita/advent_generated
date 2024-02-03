
use strict;
use warnings;

sub isPrime {
    my $n = shift;
    for (my $i = 2; $i*$i <= $n; $i++) {
        if ($n % $i == 0) {
            return 0;
        }
    }
    return 1;
}

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $input = <$fh>;
chomp $input;
close($fh);

my $b = 57*100 + 100000;
my $c = $b + 17000;
my $h = 0;

for (my $x = $b; $x <= $c; $x += 17) {
    if (!isPrime($x)) {
        $h++;
    }
}

print "$h\n";
