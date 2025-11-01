
sub MAIN {
    my $total = 0;
    for 'input.txt'.IO.lines {
        my @hist = .comb(/\-?\d+/);
        my @next = @hist;
        my $last = 0;
        while +@next {
            $last += @next[*-1];
            @next = (^(@next-1)).map: { @next[$_+1] - @next[$_] };
        }
        $total += $last;
    }
    say $total;
}
