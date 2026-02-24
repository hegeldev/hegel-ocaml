"""Conformance tests for the Hegel OCaml SDK.

These tests verify that the OCaml SDK generates values matching the
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
    """Run all conformance tests for the OCaml SDK."""
    tests = [
        BooleanConformance(binary("test_booleans")),
        IntegerConformance(binary("test_integers"), min_value=-10**9, max_value=10**9),
        FloatConformance(binary("test_floats")),
        TextConformance(binary("test_text")),
        BinaryConformance(binary("test_binary")),
        ListConformance(binary("test_lists"), min_value=-1000, max_value=1000),
        SampledFromConformance(binary("test_sampled_from")),
        DictConformance(binary("test_hashmaps")),
        # Error handling conformance tests
        StopTestOnGenerateConformance(binary("test_booleans")),
        StopTestOnMarkCompleteConformance(binary("test_booleans")),
        StopTestOnCollectionMoreConformance(binary("test_lists")),
        StopTestOnNewCollectionConformance(binary("test_lists")),
        ErrorResponseConformance(binary("test_booleans")),
        EmptyTestConformance(binary("test_booleans")),
    ]

    run_conformance_tests(tests, subtests)
