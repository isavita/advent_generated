use strict;
use warnings;

my %modules;

sub parse_input {
    my @input = @_;
    my @prefixes = ('%', '&');

    foreach my $line (@input) {
        my ($name, $destinations) = split / -> /, $line;
        my $module = {
            name => $name,
            prefix => '',
            destinations => [split /, /, $destinations],
            state => 0,
            memory => {}
        };

        foreach my $prefix (@prefixes) {
            if (substr($name, 0, 1) eq $prefix) {
                $module->{prefix} = $prefix;
                $module->{name} = substr($name, 1);
                last;
            }
        }

        $modules{$module->{name}} = $module;
    }

    foreach my $module (values %modules) {
        foreach my $dest_name (@{$module->{destinations}}) {
            if (exists $modules{$dest_name} && $modules{$dest_name}->{prefix} eq '&') {
                $modules{$dest_name}->{memory}->{$module->{name}} = 0;
            }
        }
    }
}

sub push_button {
    my ($start_pulse, $num_cycle) = @_;
    my ($cnt_low, $cnt_high) = (0, 0);
    my @pulse_queue = ();

    for (my $i = 0; $i < $num_cycle; $i++) {
        push @pulse_queue, $start_pulse;

        while (@pulse_queue) {
            my $pulse = shift @pulse_queue;
            $pulse->{value} == 0 ? $cnt_low++ : $cnt_high++;

            next unless exists $modules{$pulse->{to_name}};

            my $module = $modules{$pulse->{to_name}};
            my $new_pulse_value;

            if ($module->{prefix} eq '%') {
                if ($pulse->{value} == 0) {
                    $module->{state} = !$module->{state};
                    $new_pulse_value = $module->{state} ? 1 : 0;
                } else {
                    next;
                }
            } elsif ($module->{prefix} eq '&') {
                $module->{memory}->{$pulse->{from_name}} = $pulse->{value};
                my $is_high_for_all = 1;
                foreach my $value (values %{$module->{memory}}) {
                    if ($value == 0) {
                        $is_high_for_all = 0;
                        last;
                    }
                }
                $new_pulse_value = $is_high_for_all ? 0 : 1;
            } else {
                $new_pulse_value = $pulse->{value};
            }

            foreach my $dest_name (@{$module->{destinations}}) {
                my $new_pulse = {
                    value => $new_pulse_value,
                    from_name => $pulse->{to_name},
                    to_name => $dest_name
                };
                push @pulse_queue, $new_pulse;
            }
        }
    }

    return ($cnt_low, $cnt_high);
}

sub solve {
    my @input = @_;
    my $start_pulse = {
        value => 0,
        from_name => 'button',
        to_name => 'broadcaster'
    };
    my $num_cycle = 1000;

    parse_input(@input);
    my ($cnt_low, $cnt_high) = push_button($start_pulse, $num_cycle);

    return $cnt_low * $cnt_high;
}

open my $fh, '<', 'input.txt' or die "Could not open file 'input.txt': $!";
my @input = <$fh>;
chomp @input;
close $fh;

my $result = solve(@input);
print "$result\n";