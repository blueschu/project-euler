#!/bin/bash
echo "Running doctests on *.py..."
python3 -m doctest *.py && echo "OK"
