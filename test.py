import os
import subprocess
import re
import sys

# Exit codes standard for Lox-based languages
EX_DATAERR = 65  # Compile error
EX_SOFTWARE = 70 # Runtime error

def run_test(file_path):
    expected_outputs = []
    expected_error = None
    
    # 1. Parse expectations
    with open(file_path, 'r') as f:
        for line in f:
            # Positive expectation
            out_match = re.search(r"// expect: (.*)", line)
            if out_match:
                expected_outputs.append(out_match.group(1).strip())
            
            # Negative (Error) expectation
            err_match = re.search(r"// expect error: (.*)", line)
            if err_match:
                expected_error = err_match.group(1).strip()

    # 2. Run the BTL executable
    process = subprocess.Popen(
        ['./btl', file_path],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    stdout, stderr = process.communicate()
    exit_code = process.returncode

    # 3. Validation Logic
    
    # CASE A: We expected an error
    if expected_error:
        # Check if the error message is in stderr or stdout (depending on where you print errors)
        all_output = stdout + stderr
        if expected_error.lower() in all_output.lower():
            if exit_code != 0:
                return True, ""
            else:
                return False, f"Expected error '{expected_error}' but program exited with code 0 (Success)."
        else:
            return False, f"Expected error '{expected_error}' not found in output. Got:\n{all_output}"

    # CASE B: We expected success
    else:
        if exit_code != 0:
            return False, f"Expected success but got exit code {exit_code}. Error:\n{stderr}"
        
        actual_outputs = [line.strip() for line in stdout.splitlines() if line.strip()]
        if actual_outputs == expected_outputs:
            return True, ""
        else:
            return False, f"Output mismatch.\nExpected: {expected_outputs}\nActual:   {actual_outputs}"

def main():
    test_dir = "tests"
    if not os.path.exists(test_dir):
        print(f"Error: {test_dir} directory not found.")
        sys.exit(1)

    files = sorted([f for f in os.listdir(test_dir) if f.endswith(".btl")])
    passed = 0
    failed = 0

    print(f"--- Running {len(files)} BTL Tests ---\n")

    for file in files:
        path = os.path.join(test_dir, file)
        success, message = run_test(path)
        if success:
            print(f"  ✅ {file}")
            passed += 1
        else:
            print(f"  ❌ {file}")
            print(f"     {message}\n")
            failed += 1

    print(f"\nSummary: {passed} passed, {failed} failed.")
    sys.exit(0 if failed == 0 else 1)

if __name__ == "__main__":
    main()