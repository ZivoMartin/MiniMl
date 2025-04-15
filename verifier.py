import os
import subprocess
import sys


def main():
    path = sys.argv[1]
    def interpret (exe, path):
        process = subprocess.Popen([exe, '-parser', "extended", path], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        stdout, stderr = process.communicate()
        return (str(stdout), str(stderr))
    

    for root, _, f_names in os.walk(path):
        for f in filter(lambda f: f.endswith(".mml"), f_names):
            path = "./" + root + "/" + f
            print ("Testing: " + path)
            (output, errors) = interpret('./interpreter.out', path)
            (output_sol, _) = interpret('./interpreter_solution.out', path)
            if len(errors) != 0:
                print('Error while executing ' + path + ': ' + errors)
                return
            if output != output_sol:
                print(path, ": outputs doesn't match")
                f = open("output", "w")
                f.write(output)
                
                f = open("output_sol", "w")
                f.write(output_sol)

                return
            print ("OK for " + path)
            
main()
