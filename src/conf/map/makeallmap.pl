#!/usr/bin/perl

@cats=('rf','ppac','coin','dssd','mssd','ssd');

foreach $cat (@cats) {
    $cmd = ("perl mapmaker.pl -c=$cat");
    print "$cmd\n";
    system ($cmd);
}
