use Digest::MD5 qw(md5_hex);

my $salt;
open(my $fh, '<', 'input.txt') or die $!;
$salt = <$fh>;
close($fh);
chomp $salt;

my $keys = 0;
my $index = 0;
my %hashCache;

sub getStretchedMD5Hash {
    my $input = shift;
    return $hashCache{$input} if exists $hashCache{$input};
    my $hash = getMD5Hash($input);
    for (my $i = 0; $i < 2016; $i++) {
        $hash = getMD5Hash($hash);
    }
    $hashCache{$input} = $hash;
    return $hash;
}

sub getMD5Hash {
    my $input = shift;
    return md5_hex($input);
}

sub findTriplet {
    my $hash = shift;
    for (my $i = 0; $i < length($hash) - 2; $i++) {
        if (substr($hash, $i, 1) eq substr($hash, $i + 1, 1) && substr($hash, $i, 1) eq substr($hash, $i + 2, 1)) {
            return substr($hash, $i, 1);
        }
    }
    return "";
}

while ($keys < 64) {
    my $hash = getStretchedMD5Hash($salt . $index);
    my $triplet = findTriplet($hash);
    if ($triplet ne "") {
        for (my $i = 1; $i <= 1000; $i++) {
            my $nextHash = getStretchedMD5Hash($salt . ($index + $i));
            if (index($nextHash, $triplet x 5) != -1) {
                $keys++;
                last;
            }
        }
    }
    $index++;
}

print $index - 1 . "\n";