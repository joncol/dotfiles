#!/usr/bin/perl

use 5.012;

sub get_screen {
    my ($re) = @_;
    my $t = `xrandr | grep -PA1 "^$re connected"`;
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
my ($dp1_n_name, $dp1_n_res) = get_screen "DP-?1-[12]";
my ($hdmi_name, undef) = get_screen "HDMI-0";
my ($dp_n_name, undef) = get_screen "DP-\\d";
my ($dvid_n_name, undef) = get_screen "DVI-D-\\d";

say "Trying to identify screens...";
# Laptop setup

if ($laptop_name) {
    say "Found laptop screen: " . $laptop_name;
    `xrandr --output $laptop_name --auto`;
}

if ($dp1_n_name && $dp1_n_res eq "3440x1440") {
    say "Found work screen: " . $dp1_n_name;
    `xrandr --output $dp1_n_name --mode 3440x1440 --right-of $laptop_name`;
    $turn_off_laptop_screen = 1;
    goto FINISH;
}

if ($hdmi_name) {
    say "Found left home screen: " . $hdmi_name;
    `xrandr --output $hdmi_name --auto`;
    $left_home_screen = 1;
    $turn_off_laptop_screen = 1;
}

if ($dp1_n_name && $dp1_n_res eq "1920x1200") {
    say "Found right home screen: " . $dp1_n_name;
    if ($left_home_screen) {
        `xrandr --output $dp1_n_name --auto --right-of $hdmi_name`;
    } else {
        `xrandr --output $dp1_n_name --off`;
        `xrandr --output $dp1_n_name --auto --right-of $laptop_name`;
    }
    $turn_off_laptop_screen = 1;
}

# Desktop setup

if ($dp_n_name) {
    say "Found left home screen: " . $dp_n_name;
    `xrandr --output $dp_n_name --auto`;
    $left_home_screen = 1;
}

if ($dvid_n_name) {
    say "Found right home screen: " . $dvid_n_name;
    if ($left_home_screen && $dp_n_name) {
        `xrandr --output $dvid_n_name --auto --right-of $dp_n_name`;
    } elsif ($left_home_screen && $hdmi_name) {
        `xrandr --output $dvid_n_name --auto --right-of $hdmi_name`;
    } else {
        `xrandr --output $dvid_n_name --auto --right-of $laptop_name`;
    }
}

FINISH:

# if ($laptop_name && $turn_off_laptop_screen) {
#     `xrandr --output $laptop_name --off`;
# }
