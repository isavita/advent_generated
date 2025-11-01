
my @bots = 'input.txt'.IO.lines.map: {
    / ^ 'pos=<' ( \-? \d+ ) ',' ( \-? \d+ ) ',' ( \-? \d+ ) '>, r=' (\d+) $ /;
    { x => $0.Int, y => $1.Int, z => $2.Int, r => $3.Int }
}

my $strong = @bots.max: *<r>;
say +@bots.grep: { ($_<x> - $strong<x>).abs + ($_<y> - $strong<y>).abs + ($_<z> - $strong<z>).abs <= $strong<r> }
