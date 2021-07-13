java: clean
	javac Pigzj.java

jar: clean java
	jar cf hw3.jar Pigzj.java README.txt

clean:
	rm -f *.gz *.class *.jar
	rm -rf testdir

check: java
	bash check.sh

test: jar
	bash test_submission.sh
