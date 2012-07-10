#!/bin/bash
#
# If the contract name contains holds, thm, or unsat it should
# indeed hold, otherwise there should be a countermodel.
#
# Tries paradox, equinox and vampire (in that order)
# To remove a tool, uncomment line towards the end of this file
#


#!/bin/bash

# Kill whole testing process on Ctrl-C
trap 'exit 1' INT

# Remove all tptp files here
rm -v *.tptp

# Generate all contracts
hcc $@ -d -i -b

# Result is stored in .tmp
# Arguments
#    $1 : is a good grep
#    $2 : is a bad grep
#    $3 : says what the tool name is
function res_parser {
#    echo "Res parser $1 $2 from $3"
    if [[ `grep -i " $1" .tmp` ]]; then
        echo "OK from $3"
        return 0
    elif [[ `grep -i " $2" .tmp` ]]; then
        echo "=== FAIL from $3 ==="
        cat .tmp
        echo "=== END OF FAIL ==="
        return 0
    else
        echo "$3 timed out"
        return 1
    fi
}

# Arguments
#   $1 : Tool to run (equinox/paradox)
#   $2 : Timeout in seconds
#   $3 : Filename
#   $4 : good grep
#   $5 : bad grep
function run_koentool {
#    echo "koentool $4 $5"
    (timeout $2 $1 --no-progress --tstp $3 | grep RESULT) > .tmp
    res_parser $4 $5 $1
}

# Arguments
#   $1 : Timeout in seconds
#   $2 : Filename
#   $3 : good grep
#   $4 : bad grep
function run_vampire {
#    echo "vampire $3"
    (timeout $1 vampire_lin32 -mode casc -t $1 < $2 | grep status) > .tmp
    res_parser $3 $4 vampire
}

# Arguments
#   $1 : Timeout in seconds
#   $2 : Filename
#   $3 : good grep
#   $4 : bad grep
function run_eprover {
    (eprover -tAuto -xAuto --tptp3-format $2 --cpu-limit=$1 -s | grep status) > .tmp
    res_parser $3 $4 eprover
}

# Arguments
#   $1 : Timeout in seconds
#   $2 : Filename
#   $3 : good grep
#   $4 : bad grep
function run_z3 {
#    echo "z3 $3"
    sed 's/\$min/min/g' $2 > .z3.tmp
    (timeout $1 z3 -tptp -nw .z3.tmp | grep status) > .tmp
    res_parser $3 $4 z3
}


for FILE in `find -iname '*.tptp'`
do
    # holds=0 if it should hold, 1 otherwise
    holds=`echo $FILE | egrep '(unsat|thm|holds)'`
    # if you don't want to test both of these,
    # add an appropriate continue:
    if [[ $holds ]]; then
        # continue
        echo "$FILE, should be UNSAT"
        good="unsat"
        bad="sat"
    else
        # continue
        echo "$FILE, should be SAT"
        good="sat"
        bad="unsat"
    fi
    # Remove a tool by uncommenting a line
    run_koentool paradox 2  $FILE $good $bad ||
    run_koentool equinox 10 $FILE $good $bad ||
    run_z3               10 $FILE $good $bad ||
    run_vampire          10 $FILE $good $bad ||
    run_eprover          10 $FILE $good $bad ||
    echo "All tools timed out"
    echo
done
