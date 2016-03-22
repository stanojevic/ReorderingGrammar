
all: lib dependencies

#sudo apt-get install libperl-dev
#sudo yum install perl-devel.x86_64
lib:
	mkdir -p lib ; \
	\
	wget http://search.cpan.org/CPAN/authors/id/M/MI/MIYAGAWA/App-cpanminus-1.7040.tar.gz ; \
	tar xfvz App-cpanminus* ; \
	cd App-cpanminus* ; \
	perl Makefile.PL ; \
	make ; \
	\
	./blib/script/cpanm -l $$PWD/../lib \
		Getopt::Long::Descriptive \
		List::Util \
		Mouse \
		Data::Dump \
		Config::Simple \
		\
		App::cpanminus \
		; \
	\
	cd ../ ; \
	rm -rf App-cpanminus*

dependencies:
	mkdir dependencies; \
	cd dependencies; \
	\
	wget https://www.csie.ntu.edu.tw/~cjlin/libsvmtools/ranksvm/liblinear-ranksvm-2.1.zip; \
	unzip liblinear-ranksvm-2.1.zip ; \
	rm liblinear-ranksvm-2.1.zip ; \
	mv liblinear-ranksvm-2.1/ liblinear-ranksvm ; \
	cd liblinear-ranksvm ; \
	make ; \
	cd .. ; \
	\
	wget http://www.csie.ntu.edu.tw/~cjlin/cgi-bin/libsvm.cgi?+http://www.csie.ntu.edu.tw/~cjlin/libsvm+zip ; \
	mv *zip libsvm.zip ; \
	unzip libsvm.zip ; \
	rm libsvm.zip ; \
	mv libsvm-3.21 libsvm ; \
	cd libsvm ; \
	make ; \
	cd .. ; \
	\
	wget -O - http://kheafield.com/code/kenlm.tar.gz |tar xz ;\
	cd kenlm ; \
	./bjam -j4 ; \
	cd ..



