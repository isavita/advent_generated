
my constant MAX-ITEMS   = 1000;
my constant BLOCK-HEIGHT = 7;
my constant SHAPE-WIDTH = 5;
my constant SHAPE-HEIGHT = 6;

my @locks = [0 xx SHAPE-WIDTH] xx MAX-ITEMS;
my @keys  = [0 xx SHAPE-WIDTH] xx MAX-ITEMS;
my int $num-locks = 0;
my int $num-keys  = 0;

sub parse-lock(@b, @result) {
    for ^SHAPE-WIDTH -> int $c {
        my int $cnt = 0;
        for 1..^BLOCK-HEIGHT -> int $r {
            last if @b[$r][$c] ne '#';
            ++$cnt;
        }
        @result[$c] = $cnt;
    }
}

sub parse-key(@b, @result) {
    for ^SHAPE-WIDTH -> int $c {
        my int $cnt = 0;
        loop (my int $r = SHAPE-HEIGHT - 1; $r >= 0; --$r) {
            last if @b[$r][$c] ne '#';
            ++$cnt;
        }
        @result[$c] = $cnt;
    }
}

sub fits(@lock, @key) {
    for ^SHAPE-WIDTH -> int $i {
        return False if @lock[$i] + @key[$i] > SHAPE-HEIGHT - 1;
    }
    True
}

sub MAIN() {
    my @lines = 'input.txt'.IO.lines or die;
    my int $total-valid = 0;
    my @block;
    my int $idx = 0;

    while $idx < @lines {
        my str $line = @lines[$idx];
        ++$idx;
        next if $line.trim eq '';
        @block.push: $line.comb.list;
        ++$total-valid;
        if @block == BLOCK-HEIGHT {
            if @block[0].list eqv ('#' xx SHAPE-WIDTH).list {
                parse-lock(@block, @locks[$num-locks]);
                ++$num-locks;
            } else {
                parse-key(@block, @keys[$num-keys]);
                ++$num-keys;
            }
            @block = ();
        }
    }

    if $total-valid == 0 || $total-valid % BLOCK-HEIGHT {
        say 0;
        exit;
    }

    my int $count = 0;
    for ^$num-locks -> int $i {
        for ^$num-keys -> int $j {
            ++$count if fits(@locks[$i], @keys[$j]);
        }
    }
    say $count;
}
