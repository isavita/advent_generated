
use Digest::MD5 qw(md5_hex);

my @queue = ({x => 0, y => 0, path => ""});
my $passcode = readPasscode("input.txt");
my $path = findShortestPath($passcode);
print $path . "\n";

sub readPasscode {
    my ($filename) = @_;
    open(my $fh, '<', $filename) or die "Failed to open file: $!";
    my $passcode = <$fh>;
    close($fh);
    chomp $passcode;
    return $passcode;
}

sub findShortestPath {
    my ($passcode) = @_;
    while (@queue) {
        my $point = shift @queue;
        if ($point->{x} == 3 && $point->{y} == 3) {
            return $point->{path};
        }
        my @doors = getOpenDoors($passcode, $point->{path});
        foreach my $dir (@doors) {
            my %nextPoint = %$point;
            $nextPoint{path} .= $dir;
            if ($dir eq "U") {
                $nextPoint{y}--;
            } elsif ($dir eq "D") {
                $nextPoint{y}++;
            } elsif ($dir eq "L") {
                $nextPoint{x}--;
            } elsif ($dir eq "R") {
                $nextPoint{x}++;
            }
            if ($nextPoint{x} >= 0 && $nextPoint{x} < 4 && $nextPoint{y} >= 0 && $nextPoint{y} < 4) {
                push @queue, \%nextPoint;
            }
        }
    }
    return "No path found";
}

sub getOpenDoors {
    my ($passcode, $path) = @_;
    my $hash = md5Hash($passcode . $path);
    my @doors;
    push @doors, "U" if ord(substr($hash, 0, 1)) >= ord('b') && ord(substr($hash, 0, 1)) <= ord('f');
    push @doors, "D" if ord(substr($hash, 1, 1)) >= ord('b') && ord(substr($hash, 1, 1)) <= ord('f');
    push @doors, "L" if ord(substr($hash, 2, 1)) >= ord('b') && ord(substr($hash, 2, 1)) <= ord('f');
    push @doors, "R" if ord(substr($hash, 3, 1)) >= ord('b') && ord(substr($hash, 3, 1)) <= ord('f');
    return @doors;
}

sub md5Hash {
    my ($input) = @_;
    return md5_hex($input);
}
