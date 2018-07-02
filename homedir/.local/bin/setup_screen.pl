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

my $turn_off_laptop_screen = 0;
my $left_home_screen = 0;

# Laptop screen
my ($laptop_name, undef) = get_screen "eDP-?1";
my ($dp1_name, $dp1_res) = get_screen "DP-?1-1";
my ($hdmi2_name, undef) = get_screen "HDMI2";

if ($laptop_name) {
    say "Found laptop screen";
    `xrandr --output $laptop_name --auto`;
}

if ($dp1_name && $dp1_res eq "3840x2160") {
    say "Found work screen";
    `xrandr --output $dp1_name --mode 3840x2160 --right-of $laptop_name`;
    $turn_off_laptop_screen = 1;
    goto FINISH;
}

if ($hdmi2_name) {
    say "Found left home screen";
    `xrandr --output $hdmi2_name --auto`;
    $left_home_screen = 1;
    $turn_off_laptop_screen = 1;
}

if ($dp1_name && $dp1_res eq "1920x1200") {
    say "Found right home screen";
    if ($left_home_screen) {
        `xrandr --output $dp1_name --auto --right-of $hdmi2_name`;
    } else {
        `xrandr --output $dp1_name --auto`;
    }
    $turn_off_laptop_screen = 1;
}

FINISH:

if ($laptop_name && $turn_off_laptop_screen) {
    `xrandr --output $laptop_name --off`;
}
