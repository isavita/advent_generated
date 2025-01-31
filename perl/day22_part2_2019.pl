
use bigint;

my $size = 119315717514047;
my $iter = 101741582076661;
my $offset = 0;
my $increment = 1;

open(my $fh, '<', 'input.txt') or die "Error opening input file: $!";
while (my $line = <$fh>) {
    chomp $line;
    if ($line eq 'deal into new stack') {
        $increment *= -1;
        $offset += $increment;
    } elsif ($line =~ /^cut (-?\d+)$/) {
        $offset += $1 * $increment;
    } elsif ($line =~ /^deal with increment (\d+)$/) {
        $increment *= modpow($1, $size - 2, $size);
    }
}
close $fh;

my $finalIncr = modpow($increment, $iter, $size);
my $finalOffs = (1 - modpow($increment, $iter, $size)) * modpow(1 - $increment, $size - 2, $size) * $offset;

my $answer = (2020 * $finalIncr + $finalOffs) % $size;
$answer += $size if $answer < 0;

print $answer . "\n";

sub modpow {
    my ($base, $exp, $mod) = @_;
    my $result = 1;
    $base %= $mod;
    while ($exp > 0) {
        if ($exp % 2 == 1) {
            $result = ($result * $base) % $mod;
        }
        $base = ($base * $base) % $mod;
        $exp /= 2;
    }
    return $result;
}
