#!/usr/local/bin/jperl

print "Hello!\n";

if (! $hoge) {
    $x = 3;
    $y = 4;
}
sub hoge {
    while () {
	print "Hello";
	;
    }
}

sub baho {
    $foo = 3;
    $foo &hoge
}

&hoge "foo";
