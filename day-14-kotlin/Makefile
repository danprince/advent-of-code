deps:
	curl http://central.maven.org/maven2/org/jetbrains/kotlin/kotlin-stdlib/1.2.0/kotlin-stdlib-1.2.0.jar --output lib/kotlin-stdlib.jar
	curl https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.12.4/scala-library-2.12.4.jar --output lib/scala-library.jar

compile-scala:
	@mkdir -p lib
	scalac ../day-10-scala/*.scala -d lib/Day10.jar

compile-kotlin:
	@mkdir -p build
	kotlinc -cp lib/Day10.jar:. *.kt -d build

part-one:
	@make compile-kotlin
	java -cp "build/:lib/*" advent.day14.Part_oneKt

part-two:
	@make compile-kotlin
	java -cp "build/:lib/*" advent.day14.Part_twoKt

clean:
	rm -r build/*.class
