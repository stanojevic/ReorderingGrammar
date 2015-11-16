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
#
	wget https://bitbucket.org/robeden/trove/downloads/trove-3.1a1.zip
	unzip trove-3.1a1.zip
	mv 3.1a1/lib/trove-*.jar $(LIB)
	rm -rf 3.1a1/ trove*
#
	wget https://staff.fnwi.uva.nl/m.stanojevic/beer/beer_1.0.jar -O $(LIB)/beer_1.0.jar
#
	wget http://nlp.stanford.edu/software/phrasal/phrasal.3.4.1.tar.gz
	tar xfvz phrasal*.tar.gz
	rm phrasal*.tar.gz
	mv phrasal* $(LIB)

jar: bin
	echo "Manifest-Version: 1.0" > Manifest.txt
	echo Class-Path: `find ./$(LIB) -name \*.jar | tr "\n" " "` >> Manifest.txt
	echo Main-Class: grammar.reordering.Train >> Manifest.txt
	rm -f rg_$(RG_VERSION).jar
	jar -cfm rg_$(RG_VERSION).jar Manifest.txt -C bin .
	rm Manifest.txt

bin:
	mkdir -p bin
	$(SCALA_COMPILER) -d bin -classpath `find lib -name \*.jar| tr "\n" :` `find src -name \*.scala`

package:
	rm -f rg_$(RG_VERSION).tar.gz
	tar -zcvf rg_$(RG_VERSION).tar.gz $(LIB)/*.jar *.jar

deploy: jar
	scp -r $(LIB) *.jar rg_0.1.jar mstanoj1@laco10.science.uva.nl:/home/mstanoj1/experiments/ACL14_reordering/en_ja/playground/s.install_reordering_grammarian.b8a76e71.20150115-0315/rg
#scp -r lib *.jar rg_0.1.jar mstanoj1@laco9.science.uva.nl:/home/mstanoj1/experiments/ACL14_reordering/de-en/playground/s.install_reordering_grammarian.da0fe160.20150212-1925/rg


deploy4: jar
#scp -r lib *.jar mstanoj1@laco11.science.uva.nl:/home/mstanoj1/experiments/2015_NAACL/kftt_moses/kftt-moses-1.4
	scp -r rg_0.1.jar mstanoj1@laco11.science.uva.nl:/home/mstanoj1/experiments/2015_NAACL/kftt_moses/kftt-moses-1.4/rg_0.1.jar

deploy2: jar
#scp -r lib        mstanoj1@laco10.science.uva.nl:/home/mstanoj1/experiments/en-ja/
	scp -r rg_0.1.jar mstanoj1@laco10.science.uva.nl:/home/mstanoj1/experiments/en-ja/rg_0.1.jar

deploy3: jar
#	scp -r lib        mstanoj1@laco10.science.uva.nl:/home/mstanoj1/experiments/en-ja/
	scp -r rg_0.1.jar mstanoj1@laco10.science.uva.nl:/home/mstanoj1/experiments/en-ja/rg_0.1_2.jar
