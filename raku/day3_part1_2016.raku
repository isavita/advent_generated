
sub MAIN {
    say 'input.txt'.IO.lines.grep({
        my @s = $_.words.map(*.Int).sort;
        @s[0] + @s[1] > @s[2];
    }).elems;
}
