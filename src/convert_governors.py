import os
import shutil
import subprocess

os.chdir(os.path.dirname(os.path.abspath(__file__)))
GOV_PATH = "../data/raw/david_leip/Gov Election Data (xls)"
OUTPUT_PATH = "../data/interim/governor_data"

# requires libreoffice to be installed :-/
if not os.path.exists(OUTPUT_PATH):
    os.makedirs(OUTPUT_PATH)
for f in os.listdir(GOV_PATH):
    if f.endswith(".xlsx"):
        shutil.copyfile(os.path.join(GOV_PATH, f), os.path.join(OUTPUT_PATH, f))
    elif f.endswith(".xls"):
        subprocess.run([
            "soffice",
            "--headless",
            "--convert-to",
            "xlsx",
            os.path.join(GOV_PATH, f),
            "--outdir",
            OUTPUT_PATH
        ])

