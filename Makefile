CLASSES=src/scalate-core-1.2.jar:src/slf4j-api-1.6.0.jar:src/slf4j-nop-1.6.0.jar

all:
	mkdir -p classes
	scalac -d ./classes -classpath $(CLASSES) src/*.scala

stats:
	scala -classpath classes:$(CLASSES) GenerateReport

clean:
	rm -rf classes
	rm -rf src/*.class
