
sub MAIN {
    my $txt  = slurp 'input.txt';
    my $half = $txt.chars div 2;
    my $sum  = (0..^$txt.chars).grep(-> $i { $txt.substr($i,1) eq $txt.substr(($i+$half)%$txt.chars,1) })
                               .map(-> $i { $txt.substr($i,1) })
                               .sum;
    say $sum;
}
