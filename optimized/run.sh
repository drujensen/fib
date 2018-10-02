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
languages << Language.new("Python Cached", :optimized, "", "time python3 fib-cache.py")
languages << Language.new("Lisp Compile Time", :optimized, "sbcl --load fib-compiletime.lisp", "time ./fib-compiletime")
languages << Language.new("Lisp Local", :optimized, "sbcl --load fib-local.lisp", "time ./fib-local")
languages << Language.new("C++ Constant", :optimized, "g++-8 -O3 -o fib-const fib-constexpr.cpp", "time ./fib-const")
languages << Language.new("D Memoized", :optimized, "ldc2 -O3 -release -flto=full -of=fib-mem fib-mem.d", "time ./fib-mem")
languages << Language.new("Go Memoized", :optimized, "go build fib-mem.go", "time ./fib-mem")
languages << Language.new("Node Memoized", :optimized, "", "time node fib-mem.js")
languages << Language.new("K Memoized", :optimized, "", "time k fib-mem.k")
languages << Language.new("Lua Memoized", :optimized, "", "time lua fib-mem.lua")
languages << Language.new("Nim Memoized", :optimized, "nim cpp -d:release fib_mem.nim", "time ./fib_mem")
languages << Language.new("Nim Constant", :optimized, "nim cpp -d:release fib_const.nim", "time ./fib_cont")
languages << Language.new("Perl6 Memoized", :optimized, "", "time perl6 fib-mem.p6")
languages << Language.new("Perl Memoized", :optimized, "", "time perl fib-mem.pl")
languages << Language.new("Perl Memoized 2", :optimized, "", "time perl fib-mem2.pl")
languages << Language.new("Perl Inline", :optimized, "", "time perl fib-inline.py")
languages << Language.new("Ruby Memoized", :optimized, "", "time ruby fib-mem.rb")
languages << Language.new("Swift Memoized", :optimized, "swiftc -O -g fib-mem.swift", "time ./fib-mem")
languages << Language.new("Erlang Memoized", :optimized, "erlc +native +'{hipe,[o3]}' fib_mem.erl", "time erl -noinput -noshell -s fib_mem")
languages << Language.new("Escript Memoized", :optimized, "", "time escript fib_mem.es")
languages << Language.new("Haskell Memoized", :optimized, "ghc -O3 -o fib_mem fib_mem.hs", "time ./fib_mem")
#languages << Language.new("Haskell TCO", :optimized, "ghc -O3 -o fib_tail fib_tail.hs", "time ./fib_tail")
#languages << Language.new("Rust Memoized", :optimized, "rustc -O fib_mem.rs", "time ./fib_mem")
languages << Language.new("OCaml TCO", :optimized, "ocamlopt -O3 -o fib_tail fib_tail.ml", "time ./fib_tail")
languages << Language.new("Elixir Iterative", :optimized, "time elixir fib-iterative.exs")

languages.each do |lang|
  puts "Running #{lang.name}"
  lang.run
  puts "Time #{lang.run_time}"
end

puts "Last benchmark was ran on #{Time.now.strftime("%B %d, %Y")}"
puts ""
puts "## Optimized"
puts ""
puts "| Language | Time, s | Compile | Run |"
puts "|----------|---------|---------|-----|"
languages.select {|l| l.type == :optimized}.sort_by {|l| l.run_time}.each do |lang|
    results = []
    results << lang.name
    results << ("%.3f" % lang.run_time).rjust(8, " ")
    results << lang.compile_cmd
    results << lang.run_cmd
    puts "| #{results.join(" | ")} |"
end

