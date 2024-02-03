
use strict;
use warnings;

open(my $fh, '<', 'input.txt') or die $!;

my $input = do { local $/; <$fh> };
chomp $input;

my $nice = 0;

my $passesRule1 = sub {
    my $line = shift;
    for (my $i = 0; $i < length($line) - 2; $i++) {
        my $toMatch = substr($line, $i, 2);
        for (my $j = $i + 2; $j < length($line) - 1; $j++) {
            if (substr($line, $j, 2) eq $toMatch) {
                return 1;
            }
        }
    }
    return 0;
};

for my $line (split("\n", $input)) {
    my $rule1 = $passesRule1->($line);

    my $rule2 = 0;
    for (my $i = 0; $i < length($line) - 2; $i++) {
        if (substr($line, $i, 1) eq substr($line, $i + 2, 1)) {
            $rule2 = 1;
            last;
        }
    }

    if ($rule1 && $rule2) {
        $nice++;
    }
}

print "$nice\n";
