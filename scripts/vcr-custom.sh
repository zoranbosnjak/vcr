#!/usr/bin/env bash

# Wrapper around 'vcr custom' command, to simplify custom script execution.

# Example usage:
# --- custom vcr script ---
#! /usr/bin/env -S vcr-custom --asterix /path/tohome/ubuntu/asterix-data/xml
#
# ... the rest of the script

script=$(realpath ${@:$#})
args=${@:1:$#-1}

vcr custom --program "$script $args" --run

