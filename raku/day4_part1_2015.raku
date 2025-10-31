
use Digest::MD5;

sub MAIN() {
    my $key = (slurp "input.txt").lines[0];
    my $i = 0;
    loop {
        $i++;
        my $d = md5($key ~ $i);
        if $d[0] == 0 && $d[1] == 0 && ($d[2] +& 0xF0) == 0 {
            say $i;
            last;
        }
    }
}
