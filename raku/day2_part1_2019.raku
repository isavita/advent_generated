
sub MAIN {
    my @data = 'input.txt'.IO.slurp.split(',').map(*.Int);
    @data[1] = 12;
    @data[2] = 2;
    my $pos = 0;
    while @data[$pos] != 99 {
        given @data[$pos] {
            when 1 {
                @data[@data[$pos + 3]] = @data[@data[$pos + 1]] + @data[@data[$pos + 2]];
            }
            when 2 {
                @data[@data[$pos + 3]] = @data[@data[$pos + 1]] * @data[@data[$pos + 2]];
            }
        }
        $pos += 4;
    }
    say @data[0];
}
