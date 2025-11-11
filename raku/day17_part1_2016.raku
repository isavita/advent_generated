
use Digest::MD5;

sub MAIN() {
    my $passcode = slurp("input.txt").chomp;
    my @queue = ([0,0,""]);
    my @dirs = <U D L R>;
    my int @dx = (0,0,-1,1);
    my int @dy = (-1,1,0,0);
    while @queue {
        my ($x,$y,$path) = @queue.shift.List;
        if $x == 3 && $y == 3 {
            .say and exit given $path;
        }
        my $bytes = md5($passcode ~ $path);
        my @nibbles = ($bytes[0] +> 4, $bytes[0] +& 0x0F, $bytes[1] +> 4, $bytes[1] +& 0x0F);
        for ^4 -> $i {
            next unless @nibbles[$i] > 10;
            my ($nx,$ny) = ($x + @dx[$i], $y + @dy[$i]);
            next unless 0 <= $nx < 4 && 0 <= $ny < 4;
            @queue.push: [$nx,$ny,$path ~ @dirs[$i]];
        }
    }
}
