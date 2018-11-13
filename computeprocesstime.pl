#!/usr/bin/perl

use strict;

sub calcspeed
{
    my $speedcnt = shift;
    my $speedunit = shift;
    my $speedmult = 1;
    if( $speedunit == 'M') {
      $speedmult = 1e6;
    } elsif( $speedunit == 'K') {
      $speedmult = 1e3;
    }
    return ($speedcnt * $speedmult);
}

my $nbsalts = shift || die ('Must provide the correct salt count');
my $speed = 0;
while(<>)
{
    $speed = calcspeed($1, $2) if(/^(?:Many salts|Raw):.* real, (\d+)([MK])? c\/s virtual/);
}

die "Could not parse speed" if($speed == 0);

print (1/$speed);
print "\n";
