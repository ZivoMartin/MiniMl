import os
import subprocess
import sys

def interpret(executable: str, file_path: str) -> tuple[str, str]:
    process = subprocess.Popen(
        [executable, "-parser", "extended", "-no-full-typer", file_path],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=False
    )
    stdout_bytes, stderr_bytes = process.communicate()

    stdout = stdout_bytes.decode("utf-8", errors="replace")
    stderr = stderr_bytes.decode("utf-8", errors="replace")

    return stdout, stderr


def main():
    if len(sys.argv) < 2:
        print("Usage: python script.py <path>")
        sys.exit(1)

    root_path = sys.argv[1]

    for root, _, file_names in os.walk(root_path):
        for file_name in filter(lambda name: name.endswith(".mml"), file_names):
            file_path = os.path.join(root, file_name)
            print(f"Testing: {file_path}")

            output, errors = interpret("./interpreter.out", file_path)
            expected_output, _ = interpret("./interpreter_solution.out", file_path)

            if errors:
                print(f"Error while executing {file_path}:\n{errors}")
                return

            if output != expected_output:
                print(f"{file_path}: outputs don't match")

                with open("output", "w") as out_file:
                    out_file.write(output)

                with open("output_sol", "w") as sol_file:
                    sol_file.write(expected_output)

                return

            print(f"OK for {file_path}")


if __name__ == "__main__":
    main()
