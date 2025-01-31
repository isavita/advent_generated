
open my $f, '<', 'input.txt' or die $!;
my @buyers;
while (<$f>) {
    chomp;
    push @buyers, $_ if $_ ne '';
}
close $f;
my $total = 0;
for my $b (@buyers) {
    my $s = $b;
    for (1 .. 2000) {
        my $x = $s * 64;
        $s ^= $x;
        $s &= 0xFFFFFF;
        $x = int($s / 32);
        $s ^= $x;
        $s &= 0xFFFFFF;
        $x = $s * 2048;
        $s ^= $x;
        $s &= 0xFFFFFF;
    }
    $total += $s;
}
print $total;
