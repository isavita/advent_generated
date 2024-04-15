use strict;
use warnings;

package File {
    sub new {
        my ($class, $size) = @_;
        return bless { size => $size }, $class;
    }
}

package Directory {
    sub new {
        my ($class) = @_;
        return bless {
            files => {},
            directories => {},
        }, $class;
    }

    sub total_size {
        my ($self) = @_;
        my $size = 0;
        $size += $_->{size} for values %{$self->{files}};
        $size += $_->total_size() for values %{$self->{directories}};
        return $size;
    }
}

my $root = Directory->new();
my $current_dir = $root;
my @directory_stack = ($root);

open my $file, '<', 'input.txt' or die "Could not open file 'input.txt': $!";

while (my $line = <$file>) {
    chomp $line;
    if ($line =~ /^\$ cd (.+)$/) {
        my $path = $1;
        if ($path eq '/') {
            $current_dir = $root;
            @directory_stack = ($root);
        } elsif ($path eq '..') {
            pop @directory_stack;
            $current_dir = $directory_stack[-1];
        } else {
            $current_dir->{directories}{$path} //= Directory->new();
            $current_dir = $current_dir->{directories}{$path};
            push @directory_stack, $current_dir;
        }
    } elsif ($line =~ /^dir (.+)$/) {
        my $dir_name = $1;
        $current_dir->{directories}{$dir_name} = Directory->new();
    } elsif ($line =~ /^(\d+) (.+)$/) {
        my ($size, $file_name) = ($1, $2);
        $current_dir->{files}{$file_name} = File->new($size);
    }
}

close $file;

my $sum_sizes = 0;

sub calculate_sizes {
    my ($dir) = @_;
    my $dir_size = $dir->total_size();
    $sum_sizes += $dir_size if $dir_size <= 100000;
    calculate_sizes($_) for values %{$dir->{directories}};
}

calculate_sizes($root);

print "$sum_sizes\n";