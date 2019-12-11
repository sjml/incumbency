#!/bin/bash

cd "$(dirname "$0")"

PYTHON="../env/bin/python"

$PYTHON ./calculate_markets.py
$PYTHON ./process_unemployment.py
$PYTHON ./process_gdp.py

$PYTHON ./process_senate.py

$PYTHON ./convert_governors.py
$PYTHON ./process_governors.py

$PYTHON ./convert_presidents.py
$PYTHON ./process_presidents.py

$PYTHON ./assemble_senate.py
