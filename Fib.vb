Imports System

Function fib(ByVal n)
    If n <= 1 Then Return n
    Return fib(n - 1) + fib(n - 2)
End Function

Module Program
  Sub Main(args as String())
    Console.WriteLine(fib(47))
  End Sub
End Module
