#!/usr/bin/env ruby

class Language
  attr_accessor :ext, :name, :type, :compile_cmd, :run_cmd, :compile_time, :run_time
  def initialize(ext, name, type, compile_cmd, run_cmd)
    @ext = ext
    @name = name
    @type = type
    @compile_cmd = compile_cmd
    @run_cmd = run_cmd
    @compile_times = []
    @run_times = []
  end

  def run
    unless compile_cmd.empty?
      c_time = `{ bash -c "time #{compile_cmd}" ; } 2>&1`.split("\n").find{|s| s.include? "real"}.split("\t")[1].split(/[m,s]/)
      @compile_times << (c_time[0].to_i * 60) + c_time[1].to_f
    end
    r_time = `{ bash -c "time #{run_cmd}" ; } 2>&1`.split("\n").find{|s| s.include? "real"}.split("\t")[1].split(/[m,s]/)
    @run_times << (r_time[0].to_i * 60) + r_time[1].to_f
    `rm ./fib` if run_cmd == "./fib"
  rescue StandardError  => ex
    puts ex.message
    puts ex.backtrace.inspect
  end

  def average_compile_time
    return 0 if @compile_times.empty?
    @compile_times.inject(0, :+) / @compile_times.size.to_f
  end

  def average_run_time
    return 0 if @run_times.empty?
    @run_times.inject(0, :+) / @run_times.size.to_f
  end

  def total_time
    average_compile_time + average_run_time
  end
end

languages = []
languages << Language.new("adb", "Ada", :compiled, "gnat make -O3 -gnatp -o fib fib.adb", "./fib")
languages << Language.new("s", "Assembly", :compiled, "gcc -no-pie -O3 -o fib fib.s", "./fib")
languages << Language.new("c", "C", :compiled, "gcc -O3 -o fib fib.c", "./fib")
languages << Language.new("cpp", "C++", :compiled, "g++ -O3 -o fib fib.cpp", "./fib")
languages << Language.new("cr", "Crystal", :compiled, "crystal build --release fib.cr", "./fib")
languages << Language.new("dartc", "Dart Compiled", :compiled, "dart compile exe -o fib ./fib.dart", "./fib")
languages << Language.new("pyx", "Cython", :compiled, "cython -3 --embed -o fib.pyx.c fib.pyx && gcc -O3 -o fib fib.pyx.c $(pkg-config --cflags --libs python)", "./fib")
languages << Language.new("d", "D", :compiled, "ldc2 -O3 -release -flto=full -of=fib fib.d", "./fib")
languages << Language.new("f03", "Fortran", :compiled, "gfortran -O3 -o fib fib.f03", "./fib")
languages << Language.new("go", "Go", :compiled, "go build fib.go", "./fib")
languages << Language.new("hs", "Haskell", :compiled, "rm ./fib.o && ghc -O3 -o fib fib.hs", "./fib")
languages << Language.new("lisp", "Lisp", :compiled, "sbcl --load fib.lisp", "./fib")
languages << Language.new("nim", "Nim", :compiled, "nim c -d:release fib.nim", "./fib")
languages << Language.new("ml", "OCaml", :compiled, "ocamlopt -O3 -o fib fib.ml", "./fib")
languages << Language.new("pas", "Pascal", :compiled, "fpc -O3 -Si ./fib.pas", "./fib")
languages << Language.new("pony", "Pony", :compiled, "ponyc -s -b fib -p ./fib.pony", "./fib")
languages << Language.new("rs", "Rust", :compiled, "rustc -C opt-level=3 fib.rs", "./fib")
languages << Language.new("swift", "Swift", :compiled, "swiftc -O -g fib.swift", "./fib")
languages << Language.new("v", "V", :compiled, "v -prod -o fib fib.v", "./fib")
languages << Language.new("cbl", "Cobol", :compiled, "cobc -x -O3 -o fib ./fib.cbl", "./fib")
#languages << Language.new("qb64", "QB64", :compiled, "qb64 -x $(pwd)/fib.bas -o $(pwd)/fib", "./fib")
#languages << Language.new("emo", "Emojicode", :compiled, "emojicodec fib.emojic", "./fib")

languages << Language.new("cs", "C#", :vm, "dotnet build -c Release -o ./bin", "dotnet ./bin/fib.dll")
languages << Language.new("mono", "C# (Mono)", :vm, "mcs Fib.cs", "mono Fib.exe")
languages << Language.new("erl", "Erlang", :vm, "erlc +native +'{hipe,[o3]}' fib.erl", "erl -noinput -noshell -s fib")
languages << Language.new("java", "Java", :vm, "javac Fib.java", "java Fib")
languages << Language.new("kt", "Kotlin", :vm, "kotlinc Fib.kt -include-runtime -d Fib.jar", "java -jar Fib.jar")
languages << Language.new("scala", "Scala", :vm, "scalac Fib.scala", "scala Fib")

languages << Language.new("cljc", "Clojure", :mixed, "", "clojure -M fib.cljc")
languages << Language.new("dart", "Dart", :mixed, "", "dart fib.dart")
languages << Language.new("exs", "Elixir", :mixed, "", "ERL_COMPILER_OPTIONS='[native,{hipe, [o3]}]' elixir Fib.exs")
languages << Language.new("jl", "Julia", :mixed, "", "julia -O3 fib.jl")
languages << Language.new("js", "Node", :mixed, "", "node fib.js")
languages << Language.new("luajit", "Lua Jit", :mixed, "", "luajit fib.lua")
languages << Language.new("pypy", "Python3 (PyPy)", :mixed, "", "pypy3 fib.py")
languages << Language.new("rbjit", "Ruby (jit)", :mixed, "", "ruby --jit fib.rb")

languages << Language.new("janet", "Janet", :interpreted, "", "janet ./fib.janet")
languages << Language.new("lua", "Lua", :interpreted, "", "lua fib.lua")
languages << Language.new("php", "Php", :interpreted, "", "php fib.php")
languages << Language.new("pl", "Perl", :interpreted, "", "perl fib.pl")
languages << Language.new("py", "Python", :interpreted, "", "python fib.py")
languages << Language.new("py3", "Python3", :interpreted, "", "python3 fib.py")
languages << Language.new("r", "R", :interpreted, "", "R -f fib.r")
languages << Language.new("raku", "Raku", :interpreted, "", "rakudo fib.raku")
languages << Language.new("rb", "Ruby", :interpreted, "", "ruby fib.rb")
languages << Language.new("scm", "Scheme", :interpreted, "", "guile fib.scm")
languages << Language.new("tcl", "Tcl", :interpreted, "", "tclsh fib.tcl")
languages << Language.new("es", "Escript", :interpreted, "", "escript fib.es")

#shell scripts take forever
#languages << Language.new("sh", "Bash", :interpreted, "", "bash fib.sh")
#languages << Language.new("ps1", "Powershell", :interpreted, "", "pwsh fib.ps1")

filter = ARGV[0] ? ARGV[0].split(",") : []
count = ARGV[1] ? ARGV[1].to_i : 5
list = languages

unless (filter.empty? || filter[0] == "all")
  list = languages.select{|lang| filter.include? lang.ext}
  names = list.map(&:name).join(", ")
  puts "Filter: #{names}"
end

begin
  puts "-----------------"
  list.each do |lang|
    if [:compiled, :vm, :mixed].include? lang.type
      puts "Running #{lang.name} #{count} time(s)"
      count.times do
        print "."
        lang.run
      end
    else
      puts "Running #{lang.name} 1 time"
      print "."
      lang.run
    end
    puts ""
    puts "Average Compile #{("%.3f" % lang.average_compile_time)}"
    puts "Average Run #{("%.3f" % lang.average_run_time)}"
    puts "Total Time #{("%.3f" % lang.total_time)}"
    puts "-----------------"
  end
rescue Interrupt
end

puts "Last benchmark was ran on #{Time.now.strftime("%B %d, %Y")}"
puts ""

unless list.select {|l| l.type == :compiled}.empty?
  puts "## Natively compiled, statically typed"
  puts ""
  puts "| Language | Total | Compile | Time, s | Run | Time, s | Ext |"
  puts "|----------|-------|---------|---------|-----|---------|-----|"
  list.select {|l| l.type == :compiled}.sort_by {|l| l.total_time}.each do |lang|
      results = []
      results << lang.name
      results << ("%.3f" % lang.total_time).rjust(8, " ")
      results << lang.compile_cmd
      results << ("%.3f" % lang.average_compile_time).rjust(8, " ")
      results << lang.run_cmd
      results << ("%.3f" % lang.average_run_time).rjust(8, " ")
      results << lang.ext
      puts "| #{results.join(" | ")} |"
  end
  puts ""
end

unless list.select {|l| l.type == :vm}.empty?
  puts "## VM compiled bytecode, statically typed"
  puts ""
  puts "| Language | Total | Compile | Time, s | Run | Time, s | Ext |"
  puts "|----------|-------|---------|---------|-----|---------|-----|"
  list.select {|l| l.type == :vm}.sort_by {|l| l.total_time}.each do |lang|
      results = []
      results << lang.name
      results << ("%.3f" % lang.total_time).rjust(8, " ")
      results << lang.compile_cmd
      results << ("%.3f" % lang.average_compile_time).rjust(8, " ")
      results << lang.run_cmd
      results << ("%.3f" % lang.average_run_time).rjust(8, " ")
      results << lang.ext
      puts "| #{results.join(" | ")} |"
  end
  puts ""
end

unless list.select {|l| l.type == :mixed}.empty?
  puts "## VM compiled before execution, mixed/dynamically typed"
  puts ""
  puts "| Language | Time, s | Run | Ext |"
  puts "|----------|---------|-----|-----|"
  list.select {|l| l.type == :mixed}.sort_by {|l| l.total_time}.each do |lang|
      results = []
      results << lang.name
      results << ("%.3f" % lang.total_time).rjust(8, " ")
      results << lang.run_cmd
      results << lang.ext
      puts "| #{results.join(" | ")} |"
  end
  puts ""
end

unless list.select {|l| l.type == :interpreted}.empty?
  puts "## Interpreted, dynamically typed"
  puts ""
  puts "| Language | Time, s | Run | Ext |"
  puts "|----------|---------|-----|-----|"
  list.select {|l| l.type == :interpreted}.sort_by {|l| l.total_time}.each do |lang|
      results = []
      results << lang.name
      results << ("%.3f" % lang.total_time).rjust(8, " ")
      results << lang.run_cmd
      results << lang.ext
      puts "| #{results.join(" | ")} |"
  end
  puts ""
end

puts "## Versions"
puts "All compilers are installed using apt or asdf on Ubuntu 20.04 docker image:"
puts "|---|---|"
File.foreach(".tool-versions") do |line|
  version = line.split(" ")
  puts "| #{version[0]} | #{version[1]} |"
end
