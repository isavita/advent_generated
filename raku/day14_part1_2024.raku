
#!/usr/bin/env raku

my \W = 101;
my \H = 103;

my @robots = 'input.txt'.IO.lines.map: {
    /p\= (\d+) ',' (\d+) \s+ v\= (\-?\d+) ',' (\-?\d+)/;
    { x => $0, y => $1, vx => $2, vy => $3 }
}

@robots.map: -> \r {
    r<x> = (r<x> + 100 * r<vx>) % W;
    r<y> = (r<y> + 100 * r<vy>) % H;
}

my ($q1,$q2,$q3,$q4) = 0 xx 4;
for @robots -> \r {
    next if r<x> == 50 or r<y> == 51;
    if    r<x> < 50 && r<y> < 51 { ++$q1 }
    elsif r<x> > 50 && r<y> < 51 { ++$q2 }
    elsif r<x> < 50 && r<y> > 51 { ++$q3 }
    else                         { ++$q4 }
}

say $q1 * $q2 * $q3 * $q4;
