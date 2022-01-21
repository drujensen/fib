with Ada.Text_IO; use Ada.Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;

procedure Fib is
    function Fib (N : Long_Integer) return Long_Integer is
    begin
        if N <= 1 then
            return N;
        end if;
        return Fib (N - 1) + Fib (N - 2);
    end Fib;
begin
    Put (Fib (47));
    New_Line;
end Fib;
