
class Marble {
    has $.value is rw;
    has $.prev  is rw;
    has $.next  is rw;
}

sub read-input($file) {
    my $line = $file.IO.slurp.trim;
    my @p = $line.split(' ');
    return @p[0].Int, @p[6].Int;
}

sub play-game(Int $players, Int $last) {
    my @scores = (0) xx $players;
    my $cur = Marble.new(:value(0));
    $cur.prev = $cur;
    $cur.next = $cur;
    for 1..$last -> $m {
        if $m % 23 == 0 {
            my $p = $m % $players;
            $cur = $cur.prev for ^7;
            @scores[$p] += $m + $cur.value;
            $cur.prev.next = $cur.next;
            $cur.next.prev = $cur.prev;
            $cur = $cur.next;
        }
        else {
            $cur = $cur.next;
            my $new = Marble.new(:value($m), :prev($cur), :next($cur.next));
            $cur.next.prev = $new;
            $cur.next = $new;
            $cur = $new;
        }
    }
    @scores.max;
}

sub MAIN() {
    my ($players, $last) = read-input('input.txt');
    say play-game($players, $last);
}
