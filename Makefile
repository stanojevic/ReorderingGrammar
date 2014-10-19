LIB=lib
RG_VERSION=0.1

SCALA_VERSION=2.11.2
https://oss.sonatype.org/content/groups/public/org/scalatest/scalatest_2.11/2.2.1/scalatest_2.11-2.2.1.jar
SCALA_DIR=scala_compiler
SCALA_COMPILER=./$(SCALA_DIR)/bin/scalac

beer: lib dist

$(SCALA_DIR):
	wget http://downloads.typesafe.com/scala/$(SCALA_VERSION)/scala-$(SCALA_VERSION).tgz -O scala.tgz
	tar xvfz scala.tgz
	rm -rf scala.tgz
#	rm -rf scala-2.10.3 # we need this for the compile step
	mv scala-$(SCALA_VERSION) $(SCALA_DIR)
#
	wget http://dl.bintray.com/sbt/native-packages/sbt/0.13.5/sbt-0.13.5.tgz -O sbt.tgz
	tar xvfz sbt.tgz
	rm -rf sbt.tgz
	mv sbt/bin/* $(SCALA_DIR)/bin
	rmdir sbt/bin
	mv sbt/* $(SCALA_DIR)
	rmdir sbt

$(LIB): $(SCALA_DIR)
	mkdir -p $(LIB)
#
	cp $(SCALA_DIR)/lib/scala-library.jar $(LIB)
#
	wget http://search.maven.org/remotecontent?filepath=junit/junit/4.11/junit-4.11.jar -O $(LIB)/junit-4.11.jar
#
	wget http://repo2.maven.org/maven2/org/yaml/snakeyaml/1.13/snakeyaml-1.13.jar -O $(LIB)/snakeyaml-1.13.jar
#
	git clone https://github.com/scopt/scopt.git
	cd scopt; ../$(SCALA_DIR)/bin/sbt package ; cd -
	mv scopt/target/scala-2.11/scopt_*.jar $(LIB)
	rm -rf scopt
#
	wget https://oss.sonatype.org/content/groups/public/org/scalatest/scalatest_2.11/2.2.1/scalatest_2.11-2.2.1.jar -O $(LIB)/scalatest_2.11-2.2.1.jar

jar: bin
	echo "Manifest-Version: 1.0" > Manifest.txt
	echo Class-Path: `find ./$(LIB) -name \*.jar | tr "\n" " "` >> Manifest.txt
	echo Main-Class: beer.Evaluation >> Manifest.txt
	rm -f rg_$(RG_VERSION).jar
	jar -cfm rg_$(RG_VERSION).jar Manifest.txt -C bin .
	rm Manifest.txt

bin:
	mkdir -p bin
	$(SCALA_COMPILER) -d bin -classpath `find lib -name \*.jar| tr "\n" :` `find src -name \*.scala`
