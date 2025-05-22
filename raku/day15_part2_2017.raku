sub MAIN {
    my ($genA, $genB) = 'input.txt'.IO.lines.map(*.Int);
    my $genAFactor = 16807;
    my $genBFactor = 48271;
    my $modulus = 2147483647;
    my $matches = 0;

    for ^5_000_000 -> $i {
        loop {
            $genA = ($genA * $genAFactor) % $modulus;
            last if $genA % 4 == 0;
        }
        loop {
            $genB = ($genB * $genBFactor) % $modulus;
            last if $genB % 8 == 0;
        }
        $matches++ if ($genA +& 0xFFFF) == ($genB +& 0xFFFF);
    }
    say $matches;
}