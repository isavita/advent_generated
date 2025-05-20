
sub MAIN {
    my @instructions = 'input.txt'.IO.lines;
    my %registers;
    my $pc = 0;
    my $last-sound = 0;

    sub get-operand-value($operand) {
        if $operand ~~ / ^ '-'? \d+ $ / {
            return $operand.Int;
        } else {
            return %registers{$operand} // 0;
        }
    }

    while 0 <= $pc < @instructions.elems {
        my ($command, $op1, $op2) = @instructions[$pc].split(' ');

        my $reg = $op1;

        my $val;
        if $op2.defined {
            $val = get-operand-value($op2);
        }

        given $command {
            when 'snd' {
                $last-sound = %registers{$reg} // 0;
            }
            when 'set' {
                %registers{$reg} = $val;
            }
            when 'add' {
                %registers{$reg} = (%registers{$reg} // 0) + $val;
            }
            when 'mul' {
                %registers{$reg} = (%registers{$reg} // 0) * $val;
            }
            when 'mod' {
                %registers{$reg} = (%registers{$reg} // 0) % $val;
            }
            when 'rcv' {
                if (%registers{$reg} // 0) != 0 {
                    say $last-sound;
                    exit;
                }
            }
            when 'jgz' {
                if (%registers{$reg} // 0) > 0 {
                    $pc += $val;
                    next;
                }
            }
        }

        $pc++;
    }
}
