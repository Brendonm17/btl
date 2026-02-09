#!/usr/bin/env python3
"""
BTL Performance Comparison: VM vs Transpiled

This script:
1. Runs the test file with the interpreter (VM)
2. Transpiles the test file to C
3. Compiles the transpiled C code
4. Runs the compiled version
5. Compares and reports the results
"""

import subprocess
import sys
import os
import re
import shutil
from pathlib import Path

# Configuration
BTL_INTERPRETER = "./btl"
BTL_TRANSPILER = "./build/transpiler"
BUILD_DIR = "./build"
TEST_FILE = "tests/perf_comprehensive.btl"

# Colors for terminal output
class Colors:
    HEADER = '\033[95m'
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'

def colored(text, color):
    return f"{color}{text}{Colors.ENDC}"

def run_command(cmd, cwd=None, capture=True):
    """Run a shell command and return output"""
    try:
        result = subprocess.run(
            cmd,
            shell=True,
            cwd=cwd,
            capture_output=capture,
            text=True,
            timeout=300  # 5 minute timeout
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "Timeout"
    except Exception as e:
        return -1, "", str(e)

def parse_results(output):
    """Parse test results from output"""
    results = {}
    total_time = None

    for line in output.split('\n'):
        # Match test result lines like "1.  Arithmetic: 45.123ms (result: 12345)"
        # Also handles scientific notation like "4.9e-09ms"
        match = re.search(r'(\d+)\.\s+([^:]+):\s+([\d.eE+-]+)ms\s+\(result:\s+([^)]+)\)', line)
        if match:
            test_num = match.group(1)
            test_name = match.group(2).strip()
            time_ms = float(match.group(3))
            result_val = match.group(4)
            results[test_name] = {'time': time_ms, 'result': result_val}

        # Match total time (also handles scientific notation)
        total_match = re.search(r'Total Time:\s+([\d.eE+-]+)ms', line)
        if total_match:
            total_time = float(total_match.group(1))

    return results, total_time

def main():
    print(colored("\n" + "=" * 60, Colors.HEADER))
    print(colored("  BTL Performance Comparison: VM vs Transpiled", Colors.HEADER + Colors.BOLD))
    print(colored("=" * 60, Colors.HEADER))

    # Check prerequisites
    if not os.path.exists(BTL_INTERPRETER):
        print(colored(f"\nError: Interpreter not found at {BTL_INTERPRETER}", Colors.RED))
        print("Run 'make' first to build the interpreter.")
        sys.exit(1)

    if not os.path.exists(BTL_TRANSPILER):
        print(colored(f"\nError: Transpiler not found at {BTL_TRANSPILER}", Colors.RED))
        print("Run 'make transpiler' first to build the transpiler.")
        sys.exit(1)

    if not os.path.exists(TEST_FILE):
        print(colored(f"\nError: Test file not found at {TEST_FILE}", Colors.RED))
        sys.exit(1)

    # Ensure build directory exists
    os.makedirs(BUILD_DIR, exist_ok=True)

    # ========================================================================
    # Run VM Version
    # ========================================================================
    print(colored("\n[1/4] Running VM (Interpreter) version...", Colors.BLUE))

    code, vm_output, vm_stderr = run_command(f"{BTL_INTERPRETER} {TEST_FILE}")

    if code != 0:
        print(colored(f"Error running VM version:", Colors.RED))
        print(vm_stderr)
        sys.exit(1)

    vm_results, vm_total = parse_results(vm_output)
    print(f"      VM Total Time: {colored(f'{vm_total:.2f}ms', Colors.GREEN)}")

    # ========================================================================
    # Transpile to C
    # ========================================================================
    print(colored("\n[2/4] Transpiling to C...", Colors.BLUE))

    generated_c = f"{BUILD_DIR}/perf_generated.c"
    code, _, stderr = run_command(f"{BTL_TRANSPILER} {TEST_FILE} {generated_c}")

    if code != 0:
        print(colored(f"Error transpiling:", Colors.RED))
        print(stderr)
        sys.exit(1)

    print(f"      Generated: {generated_c}")

    # ========================================================================
    # Compile Transpiled Code
    # ========================================================================
    print(colored("\n[3/4] Compiling transpiled C code...", Colors.BLUE))

    compiled_exe = f"{BUILD_DIR}/perf_compiled"

    # Compile command - include all necessary source files
    compile_cmd = (
        f"gcc -O3 -march=native -flto -DNDEBUG "
        f"-Itranspiler -Isrc "
        f"-o {compiled_exe} "
        f"{generated_c} "
        f"transpiler/compiled.c transpiler/compiled_main.c "
        f"src/chunk.c src/compiler.c src/debug.c src/memory.c "
        f"src/native_int.c src/native_list.c src/native_math.c src/native_number.c "
        f"src/native_random.c src/native_string.c src/native_system.c "
        f"src/native_table.c src/object.c src/platform.c src/runtime.c "
        f"src/scanner.c src/table.c src/threadpool.c src/value.c src/vm.c "
        f"-lm -lpthread"
    )

    code, _, stderr = run_command(compile_cmd)

    if code != 0:
        print(colored(f"Error compiling:", Colors.RED))
        print(stderr)
        sys.exit(1)

    print(f"      Compiled: {compiled_exe}")

    # ========================================================================
    # Run Transpiled Version
    # ========================================================================
    print(colored("\n[4/4] Running Transpiled version...", Colors.BLUE))

    code, trans_output, trans_stderr = run_command(f"{compiled_exe} {TEST_FILE}")

    if code != 0:
        print(colored(f"Error running transpiled version:", Colors.RED))
        print(trans_stderr)
        sys.exit(1)

    trans_results, trans_total = parse_results(trans_output)
    print(f"      Transpiled Total Time: {colored(f'{trans_total:.2f}ms', Colors.GREEN)}")

    # ========================================================================
    # Compare Results
    # ========================================================================
    print(colored("\n" + "=" * 60, Colors.HEADER))
    print(colored("  Detailed Comparison", Colors.HEADER + Colors.BOLD))
    print(colored("=" * 60, Colors.HEADER))

    print(f"\n{'Test':<25} {'VM (ms)':<12} {'Trans (ms)':<12} {'Speedup':<10} {'Match'}")
    print("-" * 70)

    all_match = True
    speedups = []

    for test_name in vm_results:
        vm_time = vm_results[test_name]['time']
        vm_result = vm_results[test_name]['result']

        if test_name in trans_results:
            trans_time = trans_results[test_name]['time']
            trans_result = trans_results[test_name]['result']

            # Calculate speedup
            if trans_time > 0:
                speedup = vm_time / trans_time
                speedups.append(speedup)
            else:
                speedup = float('inf')

            # Check result match
            results_match = vm_result == trans_result
            if not results_match:
                all_match = False

            # Format speedup with color
            if speedup >= 2.0:
                speedup_str = colored(f"{speedup:.2f}x", Colors.GREEN)
            elif speedup >= 1.0:
                speedup_str = colored(f"{speedup:.2f}x", Colors.YELLOW)
            else:
                speedup_str = colored(f"{speedup:.2f}x", Colors.RED)

            match_str = colored("OK", Colors.GREEN) if results_match else colored("MISMATCH", Colors.RED)

            print(f"{test_name:<25} {vm_time:<12.2f} {trans_time:<12.2f} {speedup_str:<19} {match_str}")
        else:
            print(f"{test_name:<25} {vm_time:<12.2f} {'N/A':<12} {'N/A':<10} {'N/A'}")

    # ========================================================================
    # Summary
    # ========================================================================
    print(colored("\n" + "=" * 60, Colors.HEADER))
    print(colored("  Summary", Colors.HEADER + Colors.BOLD))
    print(colored("=" * 60, Colors.HEADER))

    overall_speedup = vm_total / trans_total if trans_total > 0 else float('inf')
    avg_speedup = sum(speedups) / len(speedups) if speedups else 0
    max_speedup = max(speedups) if speedups else 0
    min_speedup = min(speedups) if speedups else 0

    print(f"\n  VM Total Time:         {vm_total:.2f}ms")
    print(f"  Transpiled Total Time: {trans_total:.2f}ms")
    print(f"  Overall Speedup:       {colored(f'{overall_speedup:.2f}x', Colors.GREEN if overall_speedup >= 1 else Colors.RED)}")
    print(f"  Average Test Speedup:  {avg_speedup:.2f}x")
    print(f"  Best Speedup:          {max_speedup:.2f}x")
    print(f"  Worst Speedup:         {min_speedup:.2f}x")

    if all_match:
        print(f"\n  Result Verification:   {colored('ALL TESTS MATCH', Colors.GREEN + Colors.BOLD)}")
    else:
        print(f"\n  Result Verification:   {colored('SOME TESTS MISMATCHED', Colors.RED + Colors.BOLD)}")

    print(colored("\n" + "=" * 60 + "\n", Colors.HEADER))

    return 0 if all_match else 1

if __name__ == "__main__":
    sys.exit(main())
