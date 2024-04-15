#!/usr/bin/perl
use strict;
use warnings;

sub read_input {
    my ($filename) = @_;
    open my $fh, '<', $filename or die "Cannot open $filename: $!";
    my $line = <$fh>;
    chomp $line;
    close $fh;
    return $line;
}

sub find_next_password {
    my ($password) = @_;
    do {
        $password = increment_password($password);
    } while (!is_valid_password($password));
    return $password;
}

sub increment_password {
    my ($password) = @_;
    my @chars = reverse split //, $password;
    for (my $i = 0; $i <= $#chars; $i++) {
        if ($chars[$i] eq 'z') {
            $chars[$i] = 'a';
        } else {
            $chars[$i] = chr(ord($chars[$i]) + 1);
            last;
        }
    }
    return join '', reverse @chars;
}

sub is_valid_password {
    my ($password) = @_;
    return has_straight($password) && !contains_invalid_letters($password) && has_two_pairs($password);
}

sub has_straight {
    my ($password) = @_;
    for (my $i = 0; $i < length($password) - 2; $i++) {
        if (ord(substr($password, $i, 1)) + 1 == ord(substr($password, $i+1, 1)) &&
            ord(substr($password, $i+1, 1)) + 1 == ord(substr($password, $i+2, 1))) {
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
    my $i = 0;
    while ($i < length($password) - 1) {
        if (substr($password, $i, 1) eq substr($password, $i+1, 1)) {
            $count++;
            $i++;
        }
        $i++;
    }
    return $count >= 2;
}

my $current_password = read_input("input.txt");
my $first_new_password = find_next_password($current_password);
my $second_new_password = find_next_password($first_new_password);
print "$second_new_password\n";