#!/bin/bash

declare -a implementations=(sagittarius@0.9.4 chez@v9.5)
declare -a test_files=(test/tcp)

if [ x"${CI}" != x"" ]; then
    implementations+=(larceny@1.3)
fi

echo "Preparing for Chez Scheme"
cd test-deps/testing
./setup.sh
cd ../../

check_output() {
    local status=0
    while IFS= read -r LINE; do
	echo $LINE
	case $LINE in
	    *FAIL*) status=255 ;;
	    *Exception*) status=255 ;;
	esac
    done
    return ${status}
}

EXIT_STATUS=0

for impl in ${implementations[@]}; do
    echo Testing with ${impl}
    for file in ${test_files[@]}; do
	scheme-env run ${impl} \
		   --loadpath lib \
		   --loadpath deps/pffi/src --loadpath deps/psystem/lib \
		   --standard r6rs --program ${file}-server.scm &
	# wait until the server is running
	sleep 1
	scheme-env run ${impl} \
		   --loadpath lib \
		   --loadpath deps/pffi/src --loadpath deps/psystem/lib \
		   --loadpath test-deps/testing/lib \
		   --standard r6rs --program ${file}.scm | check_output
	case ${EXIT_STATUS} in
	    0) EXIT_STATUS=$? ;;
	esac
    done
    echo Done!
    echo
done

echo Library test status ${EXIT_STATUS}
exit ${EXIT_STATUS}
