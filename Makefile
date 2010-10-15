all:
	rm -rf classes
	mkdir classes
	scalac -d classes src/*.scala

stats:
	scala -classpath classes GenerateReport
