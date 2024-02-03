
use strict;
use warnings;

sub validatePassword {
    my ($policy, $password) = @_;
    my ($min, $max, $char) = (0, 0, '');
    if ($policy =~ /(\d+)-(\d+) (\w)/) {
        ($min, $max, $char) = ($1, $2, $3);
    }
    my $count = 0;
    foreach my $c (split //, $password) {
        $count++ if $c eq $char;
    }
    return $count >= $min && $count <= $max;
}

my $validCount = 0;
open(my $fh, '<', 'input.txt') or die $!;
while (my $line = <$fh>) {
    chomp $line;
    if (my $i = index($line, ":")) {
        my ($policy, $password) = (substr($line, 0, $i), substr($line, $i+2));
        $validCount++ if validatePassword($policy, $password);
    }
}
close($fh);

print "$validCount\n";
