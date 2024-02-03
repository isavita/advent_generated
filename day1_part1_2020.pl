
open(my $fh, '<', 'input.txt') or die $!;
my @numbers;
while (my $line = <$fh>) {
    chomp $line;
    next if $line eq "";
    push @numbers, $line;
}
close($fh);

for (my $i = 0; $i < scalar(@numbers)-1; $i++) {
    for (my $j = $i + 1; $j < scalar(@numbers); $j++) {
        if ($numbers[$i] + $numbers[$j] == 2020) {
            print $numbers[$i] * $numbers[$j] . "\n";
            exit;
        }
    }
}
