#!/usr/bin/env perl

use strict;
use warnings;

my %tag = ();

for (<>) {
    $tag{$_} = 1 for m/\[.+?\](\S+)/g;
}

print join ' ' => sort keys %tag;
