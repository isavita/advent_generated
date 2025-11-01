
sub MAIN {
    my @x = 1;
    for 'input.txt'.IO.lines {
        when /noop/ { @x.push: @x[*-1] }
        when /addx \s+ (\-?\d+)/ {
            @x.push: @x[*-1];
            @x.push: @x[*-1] + $0
        }
    }
    say sum (^@x).grep({ ($_ - 19) %% 40 }).map: { ($_ + 1) * @x[$_] }
}
