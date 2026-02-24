#!/usr/bin/env python3
"""Check that bisect_ppx code coverage is 100%.

Runs bisect-ppx-report summary and parses the output to enforce
100% line coverage on library code. Exits non-zero if coverage
is below 100% or if no coverage data is found.
"""

import re
import subprocess
import sys


def main() -> int:
    result = subprocess.run(
        ["bisect-ppx-report", "summary", "--coverage-path=_build/default/test/", "--per-file"],
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        print(f"bisect-ppx-report failed: {result.stderr}", file=sys.stderr)
        return 1

    output = result.stdout.strip()
    if not output:
        print("No coverage data found.", file=sys.stderr)
        return 1

    print(output)

    # Parse the last line which contains the project total
    # Format: "100.00 %   1/1   Project coverage"
    lines = output.strip().split("\n")
    project_line = lines[-1]

    match = re.match(r"\s*([\d.]+)\s*%", project_line)
    if not match:
        print(f"Could not parse coverage from: {project_line!r}", file=sys.stderr)
        return 1

    coverage = float(match.group(1))

    if coverage < 100.0:
        print(f"\nCoverage is {coverage:.2f}%, expected 100%.", file=sys.stderr)
        # Show uncovered files
        for line in lines[:-1]:
            file_match = re.match(r"\s*([\d.]+)\s*%\s+(\d+/\d+)\s+(.+)", line)
            if file_match and float(file_match.group(1)) < 100.0:
                print(f"  {file_match.group(3)}: {file_match.group(1)}% ({file_match.group(2)})", file=sys.stderr)
        return 1

    print(f"\nCoverage: {coverage:.2f}% — OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
