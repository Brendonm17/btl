import os
import subprocess
import re
import sys

def get_executable():
    if os.path.exists('./btl_debug'):
        return './btl_debug'
    return './btl'

def run_test(file_path, executable):
    expected_outputs = []
    expected_error = None
    
    # 1. Parse expectations
    with open(file_path, 'r') as f:
        for line in f:
            out_match = re.search(r"// expect: (.*)", line)
            if out_match:
                # We use .strip() but keep empty strings if the line was "// expect: "
                expected_outputs.append(out_match.group(1).strip())
            
            err_match = re.search(r"// expect error: (.*)", line)
            if err_match:
                expected_error = err_match.group(1).strip()

    # 2. Run the BTL executable
    # We capture stdout and stderr separately
    process = subprocess.Popen(
        [executable, file_path],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    stdout_raw, stderr_raw = process.communicate()
    exit_code = process.returncode

    # 3. Clean the output
    # Program output is ONLY what went to stdout.
    # We strip trailing whitespace from each line but keep empty lines.
    actual_outputs = [line.rstrip() for line in stdout_raw.splitlines()]

    # 4. Validation Logic
    
    # CASE A: Expected an error
    if expected_error:
        # Check both streams for the error message
        all_output = stdout_raw + stderr_raw
        if expected_error.lower() in all_output.lower():
            if exit_code != 0:
                return True, ""
            else:
                return False, f"Expected error '{expected_error}' but program exited with code 0 (Success).\n--- DEBUG TRACE (STDERR) ---\n{stderr_raw}"
        else:
            return False, f"Expected error '{expected_error}' not found.\n--- DEBUG TRACE (STDERR) ---\n{stderr_raw}"

    # CASE B: Expected success
    else:
        if exit_code != 0:
            return False, f"Expected success but got exit code {exit_code}.\n--- DEBUG TRACE (STDERR) ---\n{stderr_raw}"
        
        if actual_outputs == expected_outputs:
            return True, ""
        else:
            return False, (
                f"Output mismatch.\n"
                f"Expected: {expected_outputs}\n"
                f"Actual:   {actual_outputs}\n"
                f"--- DEBUG TRACE (STDERR) ---\n{stderr_raw}"
            )

def main():
    executable = get_executable()
    test_dir = "tests"
    
    if not os.path.exists(test_dir):
        print(f"Error: {test_dir} directory not found.")
        sys.exit(1)

    files = sorted([f for f in os.listdir(test_dir) if f.endswith(".btl")])
    passed = 0

    print(f"Using executable: {executable}")
    print(f"--- Running {len(files)} BTL Tests ---\n")

    for file in files:
        path = os.path.join(test_dir, file)
        success, message = run_test(path, executable)
        if success:
            print(f"  ✅ {file}")
            passed += 1
        else:
            print(f"  ❌ {file}")
            print(f"\n{message}\n")
            print("-" * 40)
            print(f"TEST FAILED: {file}")
            sys.exit(1) # Stop immediately on failure

    print(f"\nSummary: {passed}/{len(files)} passed.")
    sys.exit(0)

if __name__ == "__main__":
    main()