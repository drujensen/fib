program fibonacci
    implicit none

    write(*,*) fib_rec(47)
    
    contains
    recursive function fib_rec(n) result(fib)
	integer, intent(in), value :: n
	integer*8                  :: fib
 
	if (n <= 1) then
		fib = n
	else
		fib = fib_rec(n - 1) + fib_rec(n - 2)
	end if
    end function fib_rec
end program fibonacci
