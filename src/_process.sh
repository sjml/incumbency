#!/bin/bash

cd "$(dirname "$0")"

PYTHON="../env/bin/python"

$PYTHON ./process_unemployment.py

$PYTHON ./process_senate.py

$PYTHON ./convert_governors.py
$PYTHON ./process_governors.py

$PYTHON ./convert_presidents.py
$PYTHON ./process_presidents.py
