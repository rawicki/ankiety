all:
	mkdir -p classes
	scalac -d classes src/*.scala

stats:
	scala -classpath classes GenerateReport

clean:
	rm -rf classes
	rm -rf src/*.class
