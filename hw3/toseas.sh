#!/bin/bash

ssh pfahnl@lnxsrv11.seas.ucla.edu 'rm -rf ~/cs131/hw3/*'
scp Makefile \
	test_submission.sh \
	Pigzj.java \
	README.txt \
	strace.sh \
	hw3.html \
	pfahnl@lnxsrv11.seas.ucla.edu:~/cs131/hw3

scp check_seas.sh pfahnl@lnxsrv11.seas.ucla.edu:~/cs131/hw3/check.sh

echo "--- TIMING, ACCURACY, and SUBMISSION CHECKS ---"
ssh pfahnl@lnxsrv11.seas.ucla.edu 'cd ~/cs131/hw3; make check; make test'
# ssh pfahnl@lnxsrv11.seas.ucla.edu 'cd ~/cs131/hw3; make java; ./strace.sh'
