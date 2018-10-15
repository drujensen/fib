#!/usr/bin/env ruby

class Language
  attr_accessor :name, :type, :compile_cmd, :run_cmd, :compile_time, :run_time
  def initialize(name, type, compile_cmd, run_cmd)
    @name = name
    @type = type
    @compile_cmd = compile_cmd
    @run_cmd = run_cmd
  end

  def run
    unless compile_cmd.empty?
      raise "compile failed" unless system("#{compile_cmd} 2>&1")
    end
    times = `{ time #{run_cmd} ; } 2>&1`.split("\n")[3].split("\t")[1].split(/[m,s]/)
    @run_time = (times[0].to_i * 60) + times[1].to_f
  end
end

languages = []
languages << Language.new("Nim", :compiled, "nim cpp -d:release fib.nim", "time ./fib")
languages << Language.new("Crystal", :compiled, "crystal build --release fib.cr", "time ./fib")
languages << Language.new("C++", :compiled, "g++ -O3 -o fib fib.cpp", "time ./fib")
languages << Language.new("C", :compiled, "gcc -O3 -o fib fib.c", "time ./fib")
languages << Language.new("Rust", :compiled, "rustc -O fib.rs", "time ./fib")
languages << Language.new("D", :compiled, "ldc2 -O3 -release -flto=full -of=fib fib.d", "time ./fib")
languages << Language.new("Go", :compiled, "go build fib.go", "time ./fib")
languages << Language.new("Swift", :compiled, "swiftc -O -g fib.swift", "time ./fib")
languages << Language.new("OCaml", :compiled, "ocamlopt -O3 -o fib fib.ml", "time ./fib")
languages << Language.new("Haskell", :compiled, "ghc -O3 -o fib fib.hs", "time ./fib")
languages << Language.new("Fortran", :compiled, "gfortran -O3 -o fib fib.f03", "time ./fib")
languages << Language.new("Lisp", :compiled, "sbcl --load fib.lisp", "time ./fib")
languages << Language.new("Cython", :compiled, "cython --embed -o fib.pyx.c fib.pyx && gcc -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python3)", "time ./fib")
languages << Language.new("Pony", :compiled, "ponyc -s -b fib -p ./pony.fib", "time ./fib")

#languages << Language.new("Java", :vm, "javac Fib.java", "time java Fib")
#languages << Language.new("C#", :vm, "dotnet build -c Release -o ./bin", "time dotnet ./bin/fib.dll")
#languages << Language.new("C# (Mono)", :vm, "mcs fib.cs", "time mono fib.exe")
#languages << Language.new("Erlang", :vm, "erlc +native +'{hipe,[o3]}' fib.erl", "time erl -noinput -noshell -s fib")

#languages << Language.new("Dart", :mixed, "", "time dart fib.dart")
#languages << Language.new("Julia", :mixed, "", "time julia -O3 fib.jl")
#languages << Language.new("Elixir", :mixed, "", "time elixir fib.exs")
#languages << Language.new("Escript", :mixed, "", "time escript fib.es")
#languages << Language.new("Node", :mixed, "", "time node fib.js")
#languages << Language.new("Clojure", :mixed, "", "time clojure fib.cljc")

#languages << Language.new("Scheme", :interpreted, "", "time guile fib.scm")
#languages << Language.new("Ruby", :interpreted, "", "time ruby fib.rb")
#languages << Language.new("Php", :interpreted, "", "time php fib.php")
#languages << Language.new("Python", :interpreted, "", "time python fib.py")
#languages << Language.new("Python3", :interpreted, "", "time python3 fib.py")
#languages << Language.new("Perl", :interpreted, "", "time perl fib.pl")
#languages << Language.new("Perl 6", :interpreted, "", "time perl6 fib.p6")
#languages << Language.new("Tcl", :interpreted, "", "time tclsh fib.tcl")
#languages << Language.new("Lua", :interpreted, "", "time lua fib.lua")

#languages << Language.new("K", :interpreted, "", "time k fib.k")
#languages << Language.new("Bash", :interpreted, "", "time bash fib.sh")
#languages << Language.new("R", :interpreted, "", "time r -f fib.r")

languages.each do |lang|
  puts "Running #{lang.name}"
  lang.run
  puts "Time #{lang.run_time}"
end

puts "Last benchmark was ran on #{Time.now.strftime("%B %d, %Y")}"
puts ""
puts "## Natively compiled, statically typed"
puts ""
puts "| Language | Time, s  | Compile                                       | Run          |"
puts "|----------|----------|-----------------------------------------------|--------------|"
languages.select {|l| l.type == :compiled}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name.ljust(8, " ")
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.compile_cmd.ljust(45, " ")
    results << lang.run_cmd.ljust(12, " ")
    puts "| #{results.join(" | ")} |"
end

puts ""
puts "## VM compiled bytecode, statically typed"
puts ""
puts "| Language | Time, s  | Compile                              | Run                       |"
puts "|----------|----------|--------------------------------------|---------------------------|"
languages.select {|l| l.type == :vm}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name.ljust(8, " ")
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.compile_cmd.ljust(36, " ")
    results << lang.run_cmd.ljust(25, " ")
    puts "| #{results.join(" | ")} |"
end

puts ""
puts "## VM compiled before execution, mixed/dynamically typed"
puts ""
puts "| Language | Time, s  | Run                       |"
puts "|----------|----------|---------------------------|"
languages.select {|l| l.type == :mixed}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name.ljust(8, " ")
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.run_cmd.ljust(25, " ")
    puts "| #{results.join(" | ")} |"
end
puts ""
puts "NOTE: These languages include compilation time which should be taken into consideration when comparing."
puts ""
puts "## Interpreted, dynamically typed"
puts ""
puts "| Language | Time, s  | Run                       |"
puts "|----------|----------|---------------------------|"
languages.select {|l| l.type == :interpreted}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name.ljust(8, " ")
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.run_cmd.ljust(25, " ")
    puts "| #{results.join(" | ")} |"
end
puts ""
puts "NOTE: Interpreted languages have a startup time cost that should be taken into consideration when comparing."
