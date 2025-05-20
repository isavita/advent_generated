
sub main {
    my int $register5 = 0;
    my %seen;
    my int $last_unique;

    my constant $MOD = 16777216;
    my constant $FACTOR = 65899;
    my constant $MAGIC = 7586220;
    my constant $BASE = 65536;

    loop {
        my int $register3 = $register5 +| $BASE;
        $register5 = $MAGIC;

        loop {
            my int $register1 = $register3 +& 255;
            $register5 = (($register5 + $register1) % $MOD * $FACTOR) % $MOD;

            if $register3 < 256 {
                if %seen{$register5} {
                    say $last_unique;
                    exit;
                }
                %seen{$register5} = True;
                $last_unique = $register5;
                last;
            } else {
                $register3 = $register3 div 256;
            }
        }
    }
}

sub MAIN {
    main();
}
