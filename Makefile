all:
	mkdir -p classes
	scalac -d ./classes -classpath src/scalate-core-1.2.jar:src/slf4j-api-1.6.0.jar:src/slf4j-nop-1.6.0.jar src/*.scala

stats:
	scala -classpath classes:src/scalate-core-1.2.jar:src/slf4j-api-1.6.0.jar:src/slf4j-nop-1.6.0.jar GenerateReport

clean:
	rm -rf classes
	rm -rf src/*.class
