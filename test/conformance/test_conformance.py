"""Conformance tests for Hegel for OCaml.

These tests verify that Hegel for OCaml generates values matching the
statistical properties expected by the Hypothesis-based conformance framework.
"""

import os
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
    tests = [
        BooleanConformance(binary("test_booleans")),
        IntegerConformance(binary("test_integers"), min_value=-(2**62), max_value=2**62-1),
        FloatConformance(binary("test_floats")),
        TextConformance(binary("test_text"), no_surrogates=True),
        BinaryConformance(binary("test_binary")),
        ListConformance(binary("test_lists"), min_value=-1000, max_value=1000),
        SampledFromConformance(binary("test_sampled_from")),
        DictConformance(binary("test_hashmaps")),
        OneOfConformance(binary("test_one_of")),
        OriginDeduplicationConformance(binary("test_origin_deduplication")),
        # Error handling tests run against the protocol test server, which
        # does not write the per-test-case server metrics file.
        StopTestOnGenerateConformance(binary("test_booleans"), skip_server_metrics=True),
        StopTestOnMarkCompleteConformance(binary("test_booleans"), skip_server_metrics=True),
        StopTestOnCollectionMoreConformance(binary("test_lists"), skip_server_metrics=True),
        StopTestOnNewCollectionConformance(binary("test_lists"), skip_server_metrics=True),
        ErrorResponseConformance(binary("test_booleans"), skip_server_metrics=True),
        EmptyTestConformance(binary("test_booleans"), skip_server_metrics=True),
    ]

    run_conformance_tests(tests, subtests)
