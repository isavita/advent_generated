use strict;
use warnings;

sub read_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open file: $!";
    my $line = <$fh>;
    chomp $line;
    close $fh;
    return $line;
}

sub increment_password {
    my ($password) = @_;
    my $i = length($password) - 1;
    while ($i >= 0) {
        substr($password, $i, 1) = chr((ord(substr($password, $i, 1)) - ord('a') + 1) % 26 + ord('a'));
        last if substr($password, $i, 1) ne 'a';
        $i--;
    }
    return $password;
}

sub has_straight {
    my ($password) = @_;
    for my $i (0 .. length($password) - 3) {
        if (ord(substr($password, $i+1, 1)) == ord(substr($password, $i, 1)) + 1 &&
            ord(substr($password, $i+2, 1)) == ord(substr($password, $i, 1)) + 2) {
            return 1;
        }
    }
    return 0;
}

sub contains_invalid_letters {
    my ($password) = @_;
    return $password =~ /[iol]/;
}

sub has_two_pairs {
    my ($password) = @_;
    my $count = 0;
    while ($password =~ /((.)\2)/g) {
        $count++;
    }
    return $count >= 2;
}

sub is_valid_password {
    my ($password) = @_;
    return has_straight($password) && !contains_invalid_letters($password) && has_two_pairs($password);
}

sub find_next_password {
    my ($password) = @_;
    do {
        $password = increment_password($password);
    } while (!is_valid_password($password));
    return $password;
}

my $current_password = read_input("input.txt");
my $new_password = find_next_password($current_password);
print "$new_password\n";