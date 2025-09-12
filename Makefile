version=net8.0
os=$(shell uname -s)
sources=$(wildcard */*.fs */*/*.fs */*.fsproj *.sln)

ifeq ($(os), Linux)
	runtime=linux-x64
	inp=
	out=.bin
endif

ifeq ($(os), Darwin)
	runtime=osx-x64
	inp=
	out=.bin
endif

ifeq ($(shell uname -o), Cygwin)
	runtime=win-x64
	inp=.exe
	out=.exe
endif

print.ps: ${sources}
	a2ps -o $@ --font-size=10 -R --columns=1 $^

print.pdf: print.ps
	ps2pdf -o $@ $^

test:
	dotnet test Ganith.Tests/Ganith.Tests.fsproj

clean:
	rm -rf */bin */obj *.bin *.exe *.log print.pdf print.ps
