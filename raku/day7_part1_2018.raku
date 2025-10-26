
sub MAIN {
    my %steps;
    my %dep;
    for 'input.txt'.IO.lines -> $line {
        my $s = $line.substr(5,1);
        my $t = $line.substr(36,1);
        %steps{$s}=1;
        %steps{$t}=1;
        %dep{$t}{$s}=1;
    }

    my @order;
    while %steps {
        my @avail = %steps.keys.grep({ !%dep{$_} || %dep{$_}.keys ⊆ @order.Set });
        my $next  = @avail.min;
        @order.push: $next;
        %steps{$next}:delete;
    }

    put @order.join;
}
