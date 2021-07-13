test_dir = CS131-Project-Sample-Grading-Script/sample_submission
dist: clean
	tar -czf project.tgz client.py server.py
	mkdir -p submission
	cp report/report.pdf project.tgz submission
	cp submission/* $(test_dir)
check:
	cd CS131-Project-Sample-Grading-Script; \
	python3 preprocess.py; \
	echo; \
	python3 client_basic.py

clean:
	rm -f $(test_dir)/*.pdf $(test_dir)/*.py $(test_dir)/*.tgz
	rm project.tgz
