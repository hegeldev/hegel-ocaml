"""Conformance tests for Hegel for OCaml.

These tests verify that Hegel for OCaml generates values matching the
statistical properties expected by the Hypothesis-based conformance framework.
"""

from pathlib import Path

import pytest
from hegel.conformance import (
    BinaryConformance,
    BooleanConformance,
    DictConformance,
    EmptyTestConformance,
    ErrorResponseConformance,
    FloatConformance,
    IntegerConformance,
    ListConformance,
    OneOfConformance,
    OriginDeduplicationConformance,
    SampledFromConformance,
    StopTestOnCollectionMoreConformance,
    StopTestOnGenerateConformance,
    StopTestOnMarkCompleteConformance,
    StopTestOnNewCollectionConformance,
    TextConformance,
    run_conformance_tests,
)

# Path to built conformance binaries
BINARY_DIR = Path(__file__).parent.parent.parent / "_build" / "default" / "conformance"


def binary(name: str) -> Path:
    """Return the path to a conformance binary."""
    return BINARY_DIR / f"{name}.exe"


def test_conformance(subtests: pytest.Subtests) -> None:
    """Run all conformance tests for Hegel for OCaml."""
    # Error-handling tests intentionally inject server-side faults that prevent
    # the per-test-case server metrics line from being written, so they must
    # opt out of the server-metrics check.
    no_server_metrics = {"skip_server_metrics": True}

    tests = [
        BooleanConformance(binary("test_booleans")),
        IntegerConformance(
            binary("test_integers"), min_value=-(2**62), max_value=2**62 - 1
        ),
        FloatConformance(binary("test_floats")),
        TextConformance(binary("test_text"), no_surrogates=True),
        BinaryConformance(binary("test_binary")),
        ListConformance(binary("test_lists"), min_value=-1000, max_value=1000),
        SampledFromConformance(binary("test_sampled_from")),
        DictConformance(binary("test_hashmaps")),
        # Error handling conformance tests
        StopTestOnGenerateConformance(binary("test_booleans"), **no_server_metrics),
        StopTestOnMarkCompleteConformance(binary("test_booleans"), **no_server_metrics),
        StopTestOnCollectionMoreConformance(binary("test_lists"), **no_server_metrics),
        StopTestOnNewCollectionConformance(binary("test_lists"), **no_server_metrics),
        ErrorResponseConformance(binary("test_booleans"), **no_server_metrics),
        EmptyTestConformance(binary("test_booleans"), **no_server_metrics),
    ]

    run_conformance_tests(
        tests,
        subtests,
        # Conformance tests added in newer hegel-core releases that the OCaml
        # library does not yet have a dedicated conformance binary for.
        skip_tests=[
            OneOfConformance,
            OriginDeduplicationConformance,
        ],
    )
