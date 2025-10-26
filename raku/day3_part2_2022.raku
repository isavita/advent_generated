
sub MAIN {
    my @data = 'input.txt'.IO.lines;
    sub priority($s) { $s gt 'Z' ?? $s.ord - 96 !! $s.ord - 38 }
    say [+] @data.map: -> $l { my @h = $l.comb; priority ((@h[0..^@h/2] (&) @h[@h/2..*]).keys[0]) }
    say [+] @data.rotor(3).map: -> @g { priority ((@g[0].comb (&) @g[1].comb (&) @g[2].comb).keys[0]) }
}
