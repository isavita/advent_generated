
open my $fh, '<', 'input.txt' or die "Error reading file: $!";
my $genAStart = <$fh>;
my $genBStart = <$fh>;
close $fh;

my $genAFactor = 16807;
my $genBFactor = 48271;
my $modulus = 2147483647;

my $genA = $genAStart;
my $genB = $genBStart;
my $matches = 0;

for (my $i = 0; $i < 40000000; $i++) {
    $genA = ($genA * $genAFactor) % $modulus;
    $genB = ($genB * $genBFactor) % $modulus;

    if (($genA & 0xFFFF) == ($genB & 0xFFFF)) {
        $matches++;
    }
}

print "$matches\n";
