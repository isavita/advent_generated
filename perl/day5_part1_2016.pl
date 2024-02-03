
use Digest::MD5 qw(md5_hex);

open(my $fh, '<', 'input.txt') or die "Could not open file: $!";
my $doorID = <$fh>;
close($fh);

chomp($doorID);
my $password = findPassword($doorID);
print "$password\n";

sub findPassword {
    my ($doorID) = @_;
    my $password = '';
    my $i = 0;
    while (length($password) < 8) {
        my $hash = md5Hash($doorID . $i);
        if (substr($hash, 0, 5) eq '00000') {
            $password .= substr($hash, 5, 1);
        }
        $i++;
    }
    return $password;
}

sub md5Hash {
    my ($input) = @_;
    return md5_hex($input);
}
