#!/bin/bash

INPUT_DIR="../inputs/"
RELEASE=""

while getopts "hr" OPTION; do
    case $OPTION in
        h)
            echo "Usage:
run-day.sh [OPTIONS]... NUM [INPUT]

Arguments:
    - h: Flag, show this help
    - r: Flag, compile with release"
            exit 0
            ;;
        r)
                RELEASE="--release"
                ;;
        *) # getopts already printed an error message
                exit 1
                ;;
    esac
done
shift $((OPTIND-1))

NUM=$1
INPUT_FILE=${2:-"day$NUM"}

cargo run $RELEASE --bin day${NUM} -- "$INPUT_DIR/$INPUT_FILE"
