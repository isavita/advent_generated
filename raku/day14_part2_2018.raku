
unit sub MAIN;

sub compute-lps (@pattern) {
    my $N = @pattern.elems;
    my @lps = (0) xx $N;
    my $len = 0;
    my $i = 1;

    while $i < $N {
        if @pattern[$i] == @pattern[$len] {
            $len++;
            @lps[$i] = $len;
            $i++;
        } else {
            if $len != 0 {
                $len = @lps[$len - 1];
            } else {
                @lps[$i] = 0;
                $i++;
            }
        }
    }
    return @lps;
}

my $input-string = 'input.txt'.IO.slurp.trim;
my @input-sequence = $input-string.comb(/\d/).map(*.Int);
my $input-len = @input-sequence.elems;

my @scoreboard = [3, 7];
my ($elf1, $elf2) = 0, 1;

my @lps = compute-lps(@input-sequence);
my $kmp-match-len = 0;

loop {
    my $new-score = @scoreboard[$elf1] + @scoreboard[$elf2];
    my @new-digits;

    if $new-score >= 10 {
        @new-digits.push: $new-score div 10;
    }
    @new-digits.push: $new-score % 10;

    for @new-digits -> $digit {
        @scoreboard.push: $digit;

        while $kmp-match-len > 0 && $digit != @input-sequence[$kmp-match-len] {
            $kmp-match-len = @lps[$kmp-match-len - 1];
        }
        if $digit == @input-sequence[$kmp-match-len] {
            $kmp-match-len++;
        }

        if $kmp-match-len == $input-len {
            print @scoreboard.elems - $input-len;
            exit;
        }
    }

    $elf1 = ($elf1 + @scoreboard[$elf1] + 1) % @scoreboard.elems;
    $elf2 = ($elf2 + @scoreboard[$elf2] + 1) % @scoreboard.elems;
}
