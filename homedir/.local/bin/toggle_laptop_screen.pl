#!/usr/bin/perl

use 5.012;

sub get_screen {
    my ($re) = @_;
    my $t = `xrandr | grep -EA1 "^$re connected"`;
    my ($l1, $l2) = split "\n", $t;
    $l2 =~ s/^\s+//;
    my ($name) = split /\s/, $l1;
    my ($res) = split /\s/, $l2;
    return ($name, $res);
}

my $enable = $ARGV[0];
my ($laptop_name, undef) = get_screen "eDP-?1";

if ($laptop_name) {
    if ($enable) {
        say "Turning on laptop screen";
        `xrandr --output $laptop_name --auto`;
    } else {
        say "Turning off laptop screen";
        `xrandr --output $laptop_name --off`;
    }
}
