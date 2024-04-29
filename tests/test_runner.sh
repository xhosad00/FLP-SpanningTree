#!/bin/bash

# Loop through each test case
for ((i=1; i<=10; i++)); do
    # Define input and output file paths
    input="./tests/$i.in"
    output="./tests/$i.my.out"
    reference="./tests/$i.out"

    # Run the test case
    ./flp23-log < "$input" > "$output"

    # Compare the output with the reference output
    if cmp -s "$output" "$reference"; then
        echo "Test $i: PASSED"
    else
        echo "Test $i: FAILED"
    fi
done
