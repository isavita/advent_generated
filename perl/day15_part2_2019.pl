#!/usr/bin/perl
use strict;
use warnings;

sub coordKey { return "$_[0],$_[1]"; }

{
    package Intcode;
    use strict;
    use warnings;

    sub new {
        my ($class, $program_str) = @_;
        my $self = {
            memory => [],
            ip => 0,
            relativeBase => 0,
            inputs => [],
            outputs => [],
            halted => 0,
            waitingForInput => 0
        };
        bless $self, $class;

        my @codes = defined $program_str ? split /,/, $program_str : ();
        for (my $i = 0; $i < @codes; $i++) {
            $self->{memory}[$i] = 0 + $codes[$i];
        }
        return $self;
    }

    sub getMem {
        my ($self, $addr) = @_;
        die "Invalid memory address: $addr" if $addr < 0;
        return defined $self->{memory}[$addr] ? $self->{memory}[$addr] : 0;
    }

    sub setMem {
        my ($self, $addr, $value) = @_;
        die "Invalid memory address: $addr" if $addr < 0;
        $self->{memory}[$addr] = $value;
    }

    sub getParam {
        my ($self, $offset, $mode) = @_;
        my $paramAddr = $self->{ip} + $offset;
        my $paramVal = $self->getMem($paramAddr);
        if ($mode == 0) { # position
            return $self->getMem($paramVal);
        } elsif ($mode == 1) { # immediate
            return $paramVal;
        } elsif ($mode == 2) { # relative
            return $self->getMem($self->{relativeBase} + $paramVal);
        } else {
            die "Invalid parameter mode: $mode";
        }
    }

    sub setParam {
        my ($self, $offset, $mode, $value) = @_;
        my $paramAddr = $self->{ip} + $offset;
        my $paramVal = $self->getMem($paramAddr);
        if ($mode == 0) { # position
            $self->setMem($paramVal, $value);
        } elsif ($mode == 2) { # relative
            $self->setMem($self->{relativeBase} + $paramVal, $value);
        } else {
            die "Invalid target parameter mode: $mode";
        }
    }

    sub addInput {
        my ($self, $value) = @_;
        push @{$self->{inputs}}, $value;
    }

    sub getOutput {
        my ($self) = @_;
        if (@{$self->{outputs}}) {
            return shift @{$self->{outputs}};
        }
        return undef;
    }

    sub run {
        my ($self) = @_;
        $self->{waitingForInput} = 0;
        while (!$self->{halted} && !$self->{waitingForInput}) {
            my $instruction = $self->getMem($self->{ip});
            my $opcode = $instruction % 100;
            my $mode1 = int($instruction / 100) % 10;
            my $mode2 = int($instruction / 1000) % 10;
            my $mode3 = int($instruction / 10000) % 10;

            if ($opcode == 1) { # add
                my $val1 = $self->getParam(1, $mode1);
                my $val2 = $self->getParam(2, $mode2);
                $self->setParam(3, $mode3, $val1 + $val2);
                $self->{ip} += 4;
            } elsif ($opcode == 2) { # multiply
                my $val1 = $self->getParam(1, $mode1);
                my $val2 = $self->getParam(2, $mode2);
                $self->setParam(3, $mode3, $val1 * $val2);
                $self->{ip} += 4;
            } elsif ($opcode == 3) { # input
                if (@{$self->{inputs}}) {
                    my $inputVal = shift @{$self->{inputs}};
                    $self->setParam(1, $mode1, $inputVal);
                    $self->{ip} += 2;
                } else {
                    $self->{waitingForInput} = 1;
                    return "waiting";
                }
            } elsif ($opcode == 4) { # output
                my $outputVal = $self->getParam(1, $mode1);
                push @{$self->{outputs}}, $outputVal;
                $self->{ip} += 2;
                return "output";
            } elsif ($opcode == 5) { # jump-if-true
                my $val1 = $self->getParam(1, $mode1);
                my $val2 = $self->getParam(2, $mode2);
                if ($val1 != 0) { $self->{ip} = $val2; } else { $self->{ip} += 3; }
            } elsif ($opcode == 6) { # jump-if-false
                my $val1 = $self->getParam(1, $mode1);
                my $val2 = $self->getParam(2, $mode2);
                if ($val1 == 0) { $self->{ip} = $val2; } else { $self->{ip} += 3; }
            } elsif ($opcode == 7) { # less than
                my $val1 = $self->getParam(1, $mode1);
                my $val2 = $self->getParam(2, $mode2);
                $self->setParam(3, $mode3, ($val1 < $val2) ? 1 : 0);
                $self->{ip} += 4;
            } elsif ($opcode == 8) { # equals
                my $val1 = $self->getParam(1, $mode1);
                my $val2 = $self->getParam(2, $mode2);
                $self->setParam(3, $mode3, ($val1 == $val2) ? 1 : 0);
                $self->{ip} += 4;
            } elsif ($opcode == 9) { # adjust relative base
                my $val1 = $self->getParam(1, $mode1);
                $self->{relativeBase} += $val1;
                $self->{ip} += 2;
            } elsif ($opcode == 99) {
                $self->{halted} = 1;
                return "halted";
            } else {
                die "Unknown opcode: $opcode at ip ".$self->{ip};
            }
        }
        return $self->{halted} ? "halted" : ($self->{waitingForInput} ? "waiting" : "ok");
    }
}

# Main
{
    my $input_file = "input.txt";
    open my $fh, '<', $input_file or do {
        die "Error: Could not open $input_file\n";
    };
    my $program_str = do { local $/; <$fh> };
    close $fh;
    $program_str =~ s/^\s+|\s+$//g;

    my $computer = Intcode->new($program_str);

    my %grid;
    my $start_key = coordKey(0,0);
    $grid{$start_key} = '.';
    my $oxygen = { x => undef, y => undef };

    exploreMap:
    {
        sub exploreMap {
            my ($computer, $grid_ref, $x, $y, $oxygen_ref) = @_;
            my @moves = ( [0,-1], [0,1], [-1,0], [1,0] ); # N, S, W, E
            my %opposite = ( 1 => 2, 2 => 1, 3 => 4, 4 => 3 );
            for my $moveCmd (1..4) {
                my $mv = $moves[$moveCmd-1];
                my $nx = $x + $mv->[0];
                my $ny = $y + $mv->[1];
                my $nk = coordKey($nx, $ny);
                next if exists $grid_ref->{$nk};
                $computer->addInput($moveCmd);
                my $ret = $computer->run();
                my $status = $computer->getOutput();
                die "Expected output" unless defined $status;
                if ($status == 0) {
                    $grid_ref->{$nk} = '#';
                } elsif ($status == 1 || $status == 2) {
                    $grid_ref->{$nk} = '.';
                    if ($status == 2) {
                        $grid_ref->{$nk} = 'O';
                        $oxygen_ref->{x} = $nx;
                        $oxygen_ref->{y} = $ny;
                    }
                    exploreMap($computer, $grid_ref, $nx, $ny, $oxygen_ref);
                    my $backMove = $opposite{$moveCmd};
                    $computer->addInput($backMove);
                    $computer->run();
                    my $backStatus = $computer->getOutput();
                    if ($backStatus != 1 && $backStatus != 2) {
                        die sprintf("Backtrack failed from (%d,%d) to (%d,%d) with move %d, status %d",
                                    $nx, $ny, $x, $y, $backMove, (defined $backStatus ? $backStatus : -1));
                    }
                } else {
                    die "Unknown status code: $status";
                }
            }
        }
        exploreMap($computer, \%grid, 0, 0, $oxygen);
    }

    if (!defined $oxygen->{x}) {
        print "Error: Oxygen system not found during exploration.\n";
        exit 0;
    }

    sub findShortestPath {
        my ($grid_ref, $startX, $startY, $targetX, $targetY) = @_;
        my @queue = ( { x => $startX, y => $startY, dist => 0 } );
        my %visited;
        $visited{ coordKey($startX,$startY) } = 1;
        while (@queue) {
            my $cur = shift @queue;
            if ($cur->{x} == $targetX && $cur->{y} == $targetY) {
                return $cur->{dist};
            }
            my @moves = ( [0,-1], [0,1], [-1,0], [1,0] );
            for my $mv (@moves) {
                my $nx = $cur->{x} + $mv->[0];
                my $ny = $cur->{y} + $mv->[1];
                my $nk = coordKey($nx,$ny);
                next unless exists $grid_ref->{$nk};
                next if $grid_ref->{$nk} eq '#';
                next if $visited{$nk};
                $visited{$nk} = 1;
                push @queue, { x => $nx, y => $ny, dist => $cur->{dist} + 1 };
            }
        }
        return -1;
    }

    sub calculateFillTime {
        my ($grid_ref, $startX, $startY) = @_;
        my @queue = ( { x => $startX, y => $startY, time => 0 } );
        my %visited;
        $visited{ coordKey($startX,$startY) } = 1;
        my $maxTime = 0;
        while (@queue) {
            my $cur = shift @queue;
            $maxTime = $cur->{time} if $cur->{time} > $maxTime;
            my @moves = ( [0,-1], [0,1], [-1,0], [1,0] );
            for my $mv (@moves) {
                my $nx = $cur->{x} + $mv->[0];
                my $ny = $cur->{y} + $mv->[1];
                my $nk = coordKey($nx,$ny);
                next unless exists $grid_ref->{$nk};
                next unless $grid_ref->{$nk} eq '.';
                next if $visited{$nk};
                $visited{$nk} = 1;
                push @queue, { x => $nx, y => $ny, time => $cur->{time} + 1 };
            }
        }
        return $maxTime;
    }

    # Part 1
    my $shortestPath = findShortestPath(\%grid, 0, 0, $oxygen->{x}, $oxygen->{y});
    print "Part 1: Fewest movements to oxygen system: $shortestPath\n";

    # Part 2
    my $oxygenKey = coordKey($oxygen->{x}, $oxygen->{y});
    $grid{$oxygenKey} = '.'; # treat as fillable source
    my $fillTime = calculateFillTime(\%grid, $oxygen->{x}, $oxygen->{y});
    print "Part 2: Time to fill with oxygen: $fillTime\n";
}