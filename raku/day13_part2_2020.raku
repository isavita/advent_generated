
sub extended-gcd(Int \a, Int \b) {
    return (0, 1) if a == 0;
    my ($x1, $y1) = extended-gcd(b % a, a);
    ( $y1 - (b / a).floor * $x1, $x1 )
}

sub find-earliest(@ids, @offsets) {
    my Int \N = [*] @ids;
    my Int $result = 0;
    for @ids Z @offsets -> (Int \id, Int \offset) {
        my Int \ni = N div id;
        my Int \xi := extended-gcd(ni, id)[0];
        $result += (((-offset % id + id) % id * xi % N * ni % N) + N) % N;
    }
    $result % N
}

sub MAIN() {
    my @ids;
    my @offsets;
    for 'input.txt'.IO.lines[1].split(',').kv -> Int $i, Str $x {
        next if $x eq 'x';
        @ids.push:     +$x;
        @offsets.push: $i;
    }
    say find-earliest(@ids, @offsets)
}
