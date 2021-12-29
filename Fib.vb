Imports System

Function fib(ByVal n)
    If n <= 1 Then Return 1
    Return fib(n - 1) + fib(n - 2)
End Function

Module Program
  Sub Main(args as String())
    Console.WriteLine(fib(46))
  End Sub
End Module
