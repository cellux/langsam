#!/usr/bin/env python3

import argparse
import concurrent.futures
import os
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass
class TestResult:
    path: str
    returncode: int
    stdout: str
    stderr: str


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run Langsam test files, optionally in parallel."
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=max(1, (os.cpu_count() or 1)),
        help="Number of parallel test processes (default: CPU count).",
    )
    parser.add_argument(
        "--langsam",
        default="./langsam",
        help="Path to the langsam executable (default: ./langsam).",
    )
    parser.add_argument("tests", nargs="+", help="Test files to execute.")
    return parser.parse_args()


def run_one(langsam: str, test_path: str) -> TestResult:
    proc = subprocess.run(
        [langsam, test_path],
        capture_output=True,
        text=True,
        check=False,
    )
    return TestResult(
        path=test_path,
        returncode=proc.returncode,
        stdout=proc.stdout,
        stderr=proc.stderr,
    )


def main() -> int:
    args = parse_args()

    langsam_path = Path(args.langsam)
    if not langsam_path.exists():
        print(f"error: langsam executable not found: {langsam_path}", file=sys.stderr)
        return 2

    jobs = max(1, args.jobs)
    tests = list(dict.fromkeys(args.tests))
    failed: list[TestResult] = []

    if jobs == 1:
        for test_path in tests:
            result = run_one(args.langsam, test_path)
            if result.returncode != 0:
                failed.append(result)
    else:
        with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as executor:
            futures = [executor.submit(run_one, args.langsam, test) for test in tests]
            for future in concurrent.futures.as_completed(futures):
                result = future.result()
                if result.returncode != 0:
                    failed.append(result)

    if not failed:
        return 0

    print(f"{len(failed)} test(s) failed:", file=sys.stderr)
    for result in sorted(failed, key=lambda item: item.path):
        print(f"- {result.path} (exit {result.returncode})", file=sys.stderr)
        if result.stdout:
            print("  stdout:", file=sys.stderr)
            for line in result.stdout.rstrip("\n").splitlines():
                print(f"    {line}", file=sys.stderr)
        if result.stderr:
            print("  stderr:", file=sys.stderr)
            for line in result.stderr.rstrip("\n").splitlines():
                print(f"    {line}", file=sys.stderr)

    return 1


if __name__ == "__main__":
    raise SystemExit(main())
