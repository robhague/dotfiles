#!/usr/bin/env python

from __future__ import unicode_literals

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
clean_metadata = config.get("metadata", True)

def strip_output_from_cell(cell):
    if "outputs" in cell:
        if clean_output:
            cell["outputs"] = []
        elif clean_execution_count:
            for output in cell["outputs"]:
                if "execution_count" in output:
                    output["execution_count"] = None
    if clean_execution_count and "execution_count" in cell:
        cell["execution_count"] = None
    return cell

newcells = []
for cell in json_in.get("cells", []):
    metadata = cell.get('metadata', {}).get('git', {})
    if metadata.get('skip', False):
        continue
    elif metadata.get('stop', False):
        break

    # Look for the NO_GIT tag
    if any("__NO_GIT__" in line for line in cell.get("source",[])):
        continue

    newcells.append(strip_output_from_cell(cell))
json_in["cells"] = newcells

# Strip unwanted metadata keys
if clean_metadata:
    unwanted_keys = ["celltoolbar"]
    json_in["metadata"] = {k:v for k,v in json_in["metadata"].items()
                           if k not in unwanted_keys}


json.dump(json_in, sys.stdout, sort_keys=True, indent=1, separators=(",",": "),
          ensure_ascii=False)
