#!/usr/bin/env python3
"""Check that bisect_ppx code coverage is 100%.

Generates a Cobertura XML report from bisect_ppx coverage data and parses it
to enforce 100% line coverage on library code. When coverage is below 100%,
prints the exact uncovered source lines. Exits non-zero if coverage is below
100% or if no coverage data is found.
"""

import subprocess
import sys
import tempfile
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def main() -> int:
    with tempfile.NamedTemporaryFile(suffix=".xml", delete=False) as f:
        cobertura_path = f.name

    result = subprocess.run(
        [
            "bisect-ppx-report",
            "cobertura",
            "--coverage-path=_build/default/test/",
            cobertura_path,
        ],
        capture_output=True,
        text=True,
    )

    if result.returncode != 0:
        print(f"bisect-ppx-report failed: {result.stderr}", file=sys.stderr)
        return 1

    try:
        tree = ET.parse(cobertura_path)
    except ET.ParseError as e:
        print(f"Failed to parse Cobertura XML: {e}", file=sys.stderr)
        return 1

    root = tree.getroot()

    total_covered = 0
    total_lines = 0
    uncovered_by_file: dict[str, list[int]] = {}

    for cls in root.findall(".//class"):
        filename = cls.get("filename", "")
        for line in cls.findall(".//line"):
            total_lines += 1
            if int(line.get("hits", "0")) > 0:
                total_covered += 1
            else:
                uncovered_by_file.setdefault(filename, []).append(
                    int(line.get("number", "0"))
                )

    if total_lines == 0:
        print("No coverage data found.", file=sys.stderr)
        return 1

    coverage = (total_covered / total_lines) * 100

    # Print per-file summary (skip files with no instrumented lines)
    for cls in root.findall(".//class"):
        filename = cls.get("filename", "")
        lines = cls.findall(".//line")
        if not lines:
            continue
        covered = sum(1 for l in lines if int(l.get("hits", "0")) > 0)
        pct = (covered / len(lines)) * 100
        print(f"{pct:7.2f} % {covered:5d}/{len(lines):<5d} {filename}")
    print(f"{coverage:7.2f} % {total_covered:5d}/{total_lines:<5d} Project coverage")

    if uncovered_by_file:
        print(f"\nCoverage is {coverage:.2f}%, expected 100%.", file=sys.stderr)
        for filename, line_numbers in sorted(uncovered_by_file.items()):
            print(f"  {filename}:", file=sys.stderr)
            source_path = ROOT / filename
            try:
                source_lines = source_path.read_text().splitlines()
            except FileNotFoundError:
                for ln in line_numbers:
                    print(f"    line {ln} (source not found)", file=sys.stderr)
                continue
            for ln in line_numbers:
                if 1 <= ln <= len(source_lines):
                    print(
                        f"    line {ln}: {source_lines[ln - 1].rstrip()}",
                        file=sys.stderr,
                    )
                else:
                    print(f"    line {ln}", file=sys.stderr)
        return 1

    print(f"\nCoverage: {coverage:.2f}% — OK")
    return 0


if __name__ == "__main__":
    sys.exit(main())
