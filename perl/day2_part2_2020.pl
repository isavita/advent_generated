
use strict;
use warnings;

my $validCount = 0;
open(my $fh, '<', 'input.txt') or die $!;

while (my $line = <$fh>) {
    chomp $line;
    my ($policy, $password) = split /: /, $line;
    my ($min, $max, $char) = $policy =~ /(\d+)-(\d+) (\w)/;
    $min--; $max--;
    $validCount++ if (substr($password, $min, 1) eq $char) != (substr($password, $max, 1) eq $char);
}

close($fh);
print "$validCount\n";
