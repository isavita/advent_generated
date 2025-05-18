
sub MAIN {
    my $genAStart;
    my $genBStart;

    for 'input.txt'.IO.lines -> $line {
        if !defined $genAStart {
            $genAStart = $line.Int;
        } else {
            $genBStart = $line.Int;
            last;
        }
    }

    my $genAFactor = 16807;
    my $genBFactor = 48271;
    my $modulus    = 2147483647;

    my $genA = $genAStart;
    my $genB = $genBStart;
    my $matches = 0;

    for ^40000000 {
        $genA = ($genA * $genAFactor) % $modulus;
        $genB = ($genB * $genBFactor) % $modulus;

        if ($genA +& 0xFFFF) == ($genB +& 0xFFFF) {
            $matches++;
        }
    }

    say $matches;
}
