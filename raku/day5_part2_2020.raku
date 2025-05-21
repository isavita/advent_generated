
sub MAIN {
    my @seat-ids = 'input.txt'.IO.slurp.lines.map: {
        my $row = (.substr(0, 7).trans('F' => '0', 'B' => '1')).parse-base(2);
        my $col = (.substr(7, 3).trans('L' => '0', 'R' => '1')).parse-base(2);
        $row * 8 + $col;
    }

    my $max-seat-id = @seat-ids.max;
    say $max-seat-id;

    my $min-seat-id = @seat-ids.min;
    my $expected-sum = ($min-seat-id .. $max-seat-id).sum;
    my $actual-sum = @seat-ids.sum;
    say $expected-sum - $actual-sum;
}
