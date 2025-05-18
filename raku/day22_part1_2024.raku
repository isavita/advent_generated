
sub next-secret(int $s is copy) returns int {
    my int $x;
    $x = $s * 64;
    $s +^= $x;
    $s +&= 0xFFFFFF;
    $x = $s div 32;
    $s +^= $x;
    $s +&= 0xFFFFFF;
    $x = $s * 2048;
    $s +^= $x;
    $s +&= 0xFFFFFF;
    return $s;
}

unit sub MAIN {
    my @buyers = 'input.txt'.IO.lines.grep(*.trim).map(*.Int);

    my int $total = @buyers.hyper.map(-> $b {
        my int $s = $b;
        for 1..2000 {
            $s = next-secret($s);
        }
        $s
    }).sum;

    say $total;
}
