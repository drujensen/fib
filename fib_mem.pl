memoize('fib');
sub fib{
    my $number = shift;
    my $final_val;
    return undef if( $number < 0 );
    if ( $number == 0 ){
        $final_val = 0;
    } elsif ( $number == 1 ){
        $final_val = 1;
    } else {
        $final_val = fib( $number - 1 ) + fib( $number - 2 );
    }
    
    return $final_val;
}

sub memoize {
    my ($function) = @_;
 
    my $original = \&{$function};
    my %cache;
    my $sub = sub {
        my ($n) = @_;
        if (not exists $cache{$n}) {
            $cache{$n} = $original->($n);
        }
        return $cache{$n};
    };
    no strict 'refs';
    no warnings 'redefine';
    *{$function} = $sub;
}
print fib(46);
