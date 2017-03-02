#!/usr/bin/env python

# Remove outputs and execution counts from a Jupyter notebook.
# See http://pascalbugnion.net/blog/ipython-notebooks-and-git.html
import sys
import json

nb = sys.stdin.read()

json_in = json.loads(nb)

config = json_in["metadata"].get("git", {}).get("clean", {})

if config is False:
    # Metadata tells us not to clean the notebook:
    # simply send notebook, as is, to stdout.
    sys.stdout.write(nb)
    exit()

# Default behaviour is to reset execution counts but leave output intact
clean_execution_count = config.get("execution_counts", True)
clean_output = config.get("output", False)

def strip_output_from_cell(cell):
    if "outputs" in cell:
        if clean_output:
            cell["outputs"] = []
        elif clean_execution_count:
            for output in cell["outputs"]:
                output["execution_count"] = None
    if clean_execution_count and "execution_count" in cell:
        cell["execution_count"] = None

for cell in json_in.get("cells", []):
    strip_output_from_cell(cell)

json.dump(json_in, sys.stdout, sort_keys=True, indent=1, separators=(",",": "),
          ensure_ascii=False)
