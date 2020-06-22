#!/usr/bin/env ruby

class Language
  attr_accessor :name, :type, :compile_cmd, :run_cmd, :compile_time, :run_time
  def initialize(name, type, compile_cmd, run_cmd)
    @name = name
    @type = type
    @compile_cmd = compile_cmd
    @run_cmd = run_cmd
    @run_time = 0
  end

  def run
    unless compile_cmd.empty?
      raise "compile failed" unless system("#{compile_cmd} 2>/dev/null")
    end
    times = `{ bash -c "time #{run_cmd}" ; } 2>&1`.split("\n")[2].split("\t")[1].split(/[m,s]/)
    @run_time = (times[0].to_i * 60) + times[1].to_f
    `rm ./fib` if run_cmd == "./fib"
  rescue StandardError  => ex
    puts ex.message
    puts ex.backtrace.inspect
  end
end

languages = []
languages << Language.new("Assembly", :compiled, "gcc -no-pie -o fib fib-gcc-x64.s", "./fib")
languages << Language.new("C", :compiled, "gcc -fno-inline-small-functions -O3 -o fib fib.c", "./fib")
languages << Language.new("C++", :compiled, "g++ -fno-inline-small-functions -O3 -o fib fib.cpp", "./fib")
languages << Language.new("Fortran", :compiled, "gfortran -fno-inline-small-functions -O3 -o fib fib.f03", "./fib")
languages << Language.new("Pascal", :compiled, "fpc -O3 -Si ./fib.pas", "./fib")
languages << Language.new("Nim", :compiled, "nim cpp -d:release --passC:-fno-inline-small-functions fib.nim", "./fib")
languages << Language.new("Cython", :compiled, "cython --embed -o fib.pyx.c fib.pyx && gcc -fno-inline-small-functions -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python)", "./fib")
languages << Language.new("D", :compiled, 'bash -c "ldc2 -O3 -release -flto=full -of=fib fib.d"', "./fib")
languages << Language.new("V", :compiled, "v -cflags -fno-inline-small-functions -prod -o fib fib.v", "./fib")
languages << Language.new("Pony", :compiled, "ponyc -s -b fib -p ./fib.pony", "./fib")
languages << Language.new("Rust", :compiled, "rustc -C opt-level=3 -C lto=fat fib.rs", "./fib")
languages << Language.new("Swift", :compiled, "swiftc -O -g fib.swift", "./fib")
languages << Language.new("Crystal", :compiled, "crystal build --release fib.cr", "./fib")
languages << Language.new("Go", :compiled, "go build fib.go", "./fib")
languages << Language.new("OCaml", :compiled, "ocamlopt -O3 -o fib fib.ml", "./fib")
languages << Language.new("Lisp", :compiled, "sbcl --load fib.lisp", "./fib")
languages << Language.new("Haskell", :compiled, "rm ./fib.o && ghc -O3 -o fib fib.hs", "./fib")

languages << Language.new("Java", :vm, "javac Fib.java", "java Fib")
languages << Language.new("Kotlin", :vm, "kotlinc Fib.kt -include-runtime -d Fib.jar", "java -jar Fib.jar")
languages << Language.new("C#", :vm, "dotnet build -c Release -o ./bin", "dotnet ./bin/fib.dll")
languages << Language.new("C# (Mono)", :vm, "mcs Fib.cs", "mono Fib.exe")
languages << Language.new("Erlang", :vm, "erlc +native +'{hipe,[o3]}' fib.erl", "erl -noinput -noshell -s fib")

languages << Language.new("Dart", :mixed, "", "dart fib.dart")
languages << Language.new("Julia", :mixed, "", "julia -O3 fib.jl")
languages << Language.new("Escript", :mixed, "", "escript fib.es")
languages << Language.new("Lua Jit", :mixed, "", "luajit fib.lua")
languages << Language.new("Node", :mixed, "", "node fib.js")
languages << Language.new("Elixir", :mixed, "", "ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' && elixir fib.exs")
languages << Language.new("Clojure", :mixed, "", "clojure fib.cljc")

languages << Language.new("Php", :interpreted, "", "php fib.php")
languages << Language.new("Scheme", :interpreted, "", "guile fib.scm")
languages << Language.new("Ruby", :interpreted, "", "ruby fib.rb")
languages << Language.new("Lua", :interpreted, "", "lua fib.lua")
languages << Language.new("Python3", :interpreted, "", "python3 fib.py")
languages << Language.new("Python", :interpreted, "", "python fib.py")
languages << Language.new("Janet", :interpreted, "", "janet ./fib.janet")
languages << Language.new("Perl", :interpreted, "", "perl fib.pl")
languages << Language.new("Tcl", :interpreted, "", "tclsh fib.tcl")
languages << Language.new("Perl 6", :interpreted, "", "perl6 fib.p6")
#languages << Language.new("K", :interpreted, "", "k fib.k")
#languages << Language.new("R", :interpreted, "", "r -f fib.r")
#languages << Language.new("Bash", :interpreted, "", "bash fib.sh")
#languages << Language.new("Powershell", :interpreted, "", "pwsh fib.ps1")

begin
  languages.each do |lang|
    puts "Running #{lang.name}"
    lang.run
    puts "Time #{lang.run_time}"
  end
rescue Interrupt
end

puts "Last benchmark was ran on #{Time.now.strftime("%B %d, %Y")}"
puts ""
puts "## Natively compiled, statically typed"
puts ""
puts "| Language | Time, s | Compile | Run |"
puts "|----------|---------|---------|-----|"
languages.select {|l| l.type == :compiled}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.compile_cmd
    results << lang.run_cmd
    puts "| #{results.join(" | ")} |"
end

puts ""
puts "## VM compiled bytecode, statically typed"
puts ""
puts "| Language | Time, s | Compile | Run |"
puts "|----------|---------|---------|-----|"
languages.select {|l| l.type == :vm}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.compile_cmd
    results << lang.run_cmd
    puts "| #{results.join(" | ")} |"
end

puts ""
puts "## VM compiled before execution, mixed/dynamically typed"
puts ""
puts "| Language | Time, s | Run |"
puts "|----------|---------|-----|"
languages.select {|l| l.type == :mixed}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.run_cmd
    puts "| #{results.join(" | ")} |"
end
puts ""
puts "NOTE: These languages include compilation time which should be taken into consideration when comparing."
puts ""
puts "## Interpreted, dynamically typed"
puts ""
puts "| Language | Time, s | Run |"
puts "|----------|---------|-----|"
languages.select {|l| l.type == :interpreted}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.run_cmd
    puts "| #{results.join(" | ")} |"
end
puts ""
puts "NOTE: Interpreted languages have a startup time cost that should be taken into consideration when comparing."
