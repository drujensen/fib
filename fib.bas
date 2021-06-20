10 PRINT FIB~&&(46)
20 END
30 FUNCTION FIB~&& (N~&&)
    40 IF N~&& <= 1 THEN FIB~&& = 1 ELSE FIB~&& = FIB~&&(N~&& - 1) + FIB~&&(N~&& - 2)
50 END FUNCTION
