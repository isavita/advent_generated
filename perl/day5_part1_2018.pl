use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $polymer = <$fh>;
chomp $polymer;
close($fh);

sub react {
    my ($polymer) = @_;
    for (my $i = 0; $i < length($polymer) - 1; $i++) {
        if (ord(substr($polymer, $i, 1)) != ord(substr($polymer, $i + 1, 1)) &&
            (ord(substr($polymer, $i, 1)) + 32 == ord(substr($polymer, $i + 1, 1)) ||
                ord(substr($polymer, $i, 1)) - 32 == ord(substr($polymer, $i + 1, 1))) ) {
            return react(substr($polymer, 0, $i) . substr($polymer, $i + 2));
        }
    }
    return $polymer;
}

my $result = react($polymer);
print length($result) . "\n";