#!/bin/bash

# macOS can open this launcher from Finder after R has been installed.
cd "$(dirname "$0")" || exit 1
Rscript -e 'source("run_daybreak.R")'
