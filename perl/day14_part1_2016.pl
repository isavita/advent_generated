
use Digest::MD5 qw(md5_hex);

open(my $fh, '<', 'input.txt') or die "Cannot open file: $!";
my $salt = <$fh>;
close($fh);

chomp($salt);
my $keys = 0;
my $index = 0;

while ($keys < 64) {
    my $hash = getMD5Hash($salt . $index);
    my $triplet = findTriplet($hash);
    if ($triplet ne "") {
        for (my $i = 1; $i <= 1000; $i++) {
            my $nextHash = getMD5Hash($salt . ($index + $i));
            if (index($nextHash, $triplet x 5) != -1) {
                $keys++;
                last;
            }
        }
    }
    $index++;
}

print $index - 1 . "\n";

sub getMD5Hash {
    my ($input) = @_;
    return md5_hex($input);
}

sub findTriplet {
    my ($hash) = @_;
    for (my $i = 0; $i < length($hash) - 2; $i++) {
        if (substr($hash, $i, 1) eq substr($hash, $i + 1, 1) && substr($hash, $i, 1) eq substr($hash, $i + 2, 1)) {
            return substr($hash, $i, 1);
        }
    }
    return "";
}
