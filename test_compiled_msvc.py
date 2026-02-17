"""
BTL Transpiled Test Suite Runner (Windows/MSVC)

For each .btl test file:
  1. Transpile to C using build/transpiler.exe
  2. Compile the generated C with MSVC (cl.exe) against prebuilt btl.lib
  3. Run the resulting binary and check expected output

Prerequisites:
  - Run build_msvc.bat all (builds btl.exe and build/transpiler.exe)
  - MSVC environment must be set up (vcvarsall.bat)
"""

import os
import subprocess
import re
import sys
import glob as glob_mod

TRANSPILER = os.path.join("build", "transpiler.exe")
GENERATED_C = os.path.join("build", "generated.c")
COMPILED_EXE = os.path.join("build", "btl_compiled.exe")
BTL_LIB = os.path.join("build", "btl_vm.lib")
MAIN_OBJ = os.path.join("build", "obj", "compiled_main.obj")
BTL_OBJ_DIR = os.path.join("build", "obj")

# MSVC compile flags
CFLAGS = "/nologo /O2 /W3 /utf-8 /MD /D_CRT_SECURE_NO_WARNINGS /Isrc /Itranspiler"
CFLAGS += " /wd4244 /wd4267 /wd4090 /wd4996"


def build_vm_lib():
    """Build a static library from VM sources + compiled.c (NOT compiled_main.c)."""
    if os.path.exists(BTL_LIB) and os.path.exists(MAIN_OBJ):
        return True

    print("Building btl_vm.lib (one-time)...")
    os.makedirs(BTL_OBJ_DIR, exist_ok=True)

    src_files = glob_mod.glob("src/*.c")
    lib_obj_files = []

    for src in src_files:
        name = os.path.splitext(os.path.basename(src))[0]
        obj = os.path.join(BTL_OBJ_DIR, f"{name}.obj")
        lib_obj_files.append(obj)
        result = subprocess.run(
            f'cl {CFLAGS} /c "{src}" /Fo"{obj}"',
            capture_output=True, text=True, timeout=30, shell=True
        )
        if result.returncode != 0:
            print(f"  Failed to compile {src}: {result.stdout[:200]}")
            return False

    # compiled.c goes into the lib (runtime support)
    for src in ["transpiler\\compiled.c"]:
        name = os.path.splitext(os.path.basename(src))[0]
        obj = os.path.join(BTL_OBJ_DIR, f"{name}.obj")
        lib_obj_files.append(obj)
        result = subprocess.run(
            f'cl {CFLAGS} /c "{src}" /Fo"{obj}"',
            capture_output=True, text=True, timeout=30, shell=True
        )
        if result.returncode != 0:
            print(f"  Failed to compile {src}: {result.stdout[:200]}")
            return False

    # compiled_main.c is compiled separately (has main(), links with each test)
    result = subprocess.run(
        f'cl {CFLAGS} /c "transpiler\\compiled_main.c" /Fo"{MAIN_OBJ}"',
        capture_output=True, text=True, timeout=30, shell=True
    )
    if result.returncode != 0:
        print(f"  Failed to compile compiled_main.c: {result.stdout[:200]}")
        return False

    # Create static library (without main)
    obj_str = " ".join(f'"{o}"' for o in lib_obj_files)
    result = subprocess.run(
        f'lib /nologo /out:"{BTL_LIB}" {obj_str}',
        capture_output=True, text=True, timeout=30, shell=True
    )
    if result.returncode != 0:
        print(f"  Failed to create lib: {result.stdout[:200]}")
        return False

    print("  btl_vm.lib ready.\n")
    return True


def run_test(file_path):
    """Run a single transpiled test. Returns (success, message)."""
    expected_outputs = []
    expected_error = None

    with open(file_path, "r") as f:
        for line in f:
            out_match = re.search(r"// expect: (.*)", line)
            if out_match:
                expected_outputs.append(out_match.group(1).strip())
            err_match = re.search(r"// expect error: (.*)", line)
            if err_match:
                expected_error = err_match.group(1).strip()

    # Step 1: Transpile
    result = subprocess.run(
        [TRANSPILER, file_path, GENERATED_C],
        capture_output=True, text=True, timeout=30
    )
    if result.returncode != 0:
        # If we expected a compile error, check if the transpiler's error matches
        if expected_error:
            all_output = result.stdout + result.stderr
            if expected_error.lower() in all_output.lower():
                return True, ""
            else:
                return False, f"Transpile failed but error '{expected_error}' not found. stderr: {result.stderr[:200]}"
        return False, f"Transpile failed (exit {result.returncode}): {result.stderr[:300]}"

    # If we expected a compile error but transpilation succeeded, that's unexpected
    # (the runtime may still produce the error, so continue to run)

    # Step 2: Compile generated.c and link against btl_vm.lib + compiled_main.obj
    gen_obj = os.path.join("build", "generated.obj")
    compile_cmd = f'cl {CFLAGS} /c "{GENERATED_C}" /Fo"{gen_obj}"'
    result = subprocess.run(
        compile_cmd,
        capture_output=True, text=True, timeout=60, shell=True
    )
    if result.returncode != 0:
        errors = [l for l in result.stdout.splitlines() if "error" in l.lower()]
        return False, f"Compile failed: {chr(10).join(errors[:5])}"

    # Link: generated.obj + compiled_main.obj + btl_vm.lib
    link_cmd = f'link /nologo /out:"{COMPILED_EXE}" "{gen_obj}" "{MAIN_OBJ}" "{BTL_LIB}"'
    result = subprocess.run(
        link_cmd,
        capture_output=True, text=True, timeout=30, shell=True
    )
    if result.returncode != 0:
        errors = [l for l in result.stdout.splitlines() if "error" in l.lower()]
        return False, f"Link failed: {chr(10).join(errors[:5])}"

    # Step 3: Run
    process = subprocess.Popen(
        [COMPILED_EXE, file_path],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    stdout_raw, stderr_raw = process.communicate(timeout=30)
    exit_code = process.returncode

    actual_outputs = [line.rstrip() for line in stdout_raw.splitlines()]

    if expected_error:
        all_output = stdout_raw + stderr_raw
        if expected_error.lower() in all_output.lower():
            if exit_code != 0:
                return True, ""
            else:
                return False, f"Expected error but exit code 0"
        else:
            return False, f"Expected error '{expected_error}' not found. stderr: {stderr_raw[:200]}"
    else:
        if exit_code != 0:
            return False, f"Exit code {exit_code}. stderr: {stderr_raw[:200]}"
        if actual_outputs == expected_outputs:
            return True, ""
        else:
            return False, (
                f"Output mismatch.\n"
                f"Expected: {expected_outputs}\n"
                f"Actual:   {actual_outputs}\n"
                f"stderr: {stderr_raw[:300]}"
            )


def main():
    test_dir = "tests"
    if not os.path.exists(test_dir):
        print(f"Error: {test_dir} directory not found.")
        sys.exit(1)

    if not os.path.exists(TRANSPILER):
        print(f"Error: {TRANSPILER} not found. Run build_msvc.bat all first.")
        sys.exit(1)

    # Build VM lib once
    if not build_vm_lib():
        print("ERROR: Failed to build btl_vm.lib")
        sys.exit(1)

    files = sorted([
        f for f in os.listdir(test_dir)
        if f.endswith(".btl")
        and not f.startswith("benchmark_")
        and not f.startswith("perf_")
    ])

    # Skip tests that require features not available in transpiled mode
    skip_set = set()

    passed = 0
    failed = 0
    skipped = 0
    failures = []

    print(f"--- Running {len(files)} Transpiled Tests ---\n")

    for file in files:
        if file in skip_set:
            print(f"  SKIP {file}")
            skipped += 1
            continue

        path = os.path.join(test_dir, file)
        try:
            success, message = run_test(path)
            if success:
                print(f"  PASS {file}")
                passed += 1
            else:
                print(f"  FAIL {file}: {message[:200]}")
                failed += 1
                failures.append((file, message))
        except subprocess.TimeoutExpired:
            print(f"  TIMEOUT {file}")
            failed += 1
            failures.append((file, "TIMEOUT"))
        except Exception as e:
            print(f"  ERROR {file}: {e}")
            failed += 1
            failures.append((file, str(e)))

    # Cleanup
    for f in ["build/generated.obj", "build/generated.c"]:
        if os.path.exists(f):
            os.remove(f)

    print(f"\nSummary: {passed} passed, {failed} failed, {skipped} skipped out of {len(files)} tests")
    if failures:
        print(f"\nFailed tests:")
        for f, m in failures:
            print(f"  - {f}: {m[:300]}")
        sys.exit(1)
    sys.exit(0)


if __name__ == "__main__":
    main()
