
sub MAIN {
    my @stones = 'input.txt'.IO.slurp.words;

    for ^25 {
        my @next;
        for @stones -> $s {
            if $s eq '0' {
                @next.push: '1';
            }
            elsif $s.chars %% 2 {
                my $mid = $s.chars div 2;
                @next.push: $s.substr(0, $mid).subst(/^0+/, '') || '0';
                @next.push: $s.substr($mid).subst(/^0+/, '') || '0';
            }
            else {
                @next.push: (0 + $s * 2024).Str;
            }
        }
        @stones = @next;
    }

    say +@stones;
}
