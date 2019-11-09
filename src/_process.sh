#!/bin/bash

cd "$(dirname "$0")"

PYTHON="../env/bin/python"

$PYTHON ./process_senate.py

$PYTHON ./convert_governors.py
$PYTHON ./process_governors.py
