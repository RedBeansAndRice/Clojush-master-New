import os, stat, shutil, sys, time

##########################################################################
# Settings

tomsDef = ["compare-string-lengths",
"double-letters",
"replace-space-with-newline",
"string-lengths-backwards",
"last-index-of-zero",
"vector-average",
"mirror-image",
"x-word-lines",
"negative-to-zero",
"scrabble-score",
"smallest",
"syllables"]

rest = ["replace-space-with-newline",
"string-lengths-backwards",
"vector-average",
"mirror-image",
"x-word-lines",
"negative-to-zero",
"scrabble-score",
"smallest",
"syllables"]

uMADargs = " :genetic-operator-probabilities \"{:uniform-addition-and-deletion 1}\" :uniform-addition-and-deletion-rate 0.09"
batch_tag = sys.argv[1]

number_runs = int(sys.argv[2])

# Don't change this
starting_directory = os.getcwd()

for launchArgs in sys.argv[3:]:

    

    for problemArgs in rest:

        # Change this to the root directory where output should be placed
        output_directory = "/home/mlg11/results/"

        output_directory = output_directory + launchArgs + problemArgs + batch_tag

        # Don't change this
        clojush_directory = output_directory + "/Clojush"
        

        example_file = "clojush.problems.software."
        example_file = example_file + problemArgs + " :parent-selection :" + launchArgs + uMADargs

        title_string = problemArgs + " -- " +launchArgs

        description = """This description will be stored in a file alongside the logs from the runs. This is just a test using the odd problem.
        """

        ##########################################################################
        # Uncomment the following if you want to print timings in the logs
        #example_file += " :print-timings true"

        ##########################################################################
        # Probably don't change these
        output_prefix = "log"
        output_postfix = ".txt"

        command = "/share/apps/bin/lein with-profiles production trampoline run " + example_file

        service_tag = "tom"

        ##########################################################################
        # You don't need to change anything below here

        # Check to make sure directory doesn't exist; if not, create it
        if output_directory[-1] != "/":
            output_directory += "/"
        if os.path.isdir(output_directory):
            raise RuntimeError("Output directory already exists")

        os.mkdir(output_directory)

        # Make description file
        description_file_string = output_directory + "description.txt"
        description_f = open(description_file_string, "w")

        description_f.writelines("COMMAND:\n" + command + "\n\nTRACTOR TITLE:\n" + title_string + "\n\nDESCRIPTION:\n" + description)
        description_f.close()

        # Copy this Clojush directory to output directory.
        shutil.copytree(starting_directory, clojush_directory)

        # Make alf file
        alf_file_string = output_directory + "clojush_runs.alf"
        alf_f = open(alf_file_string, "w")

        alfcode = """##AlfredToDo 3.0
        Job -title {%s} -subtasks {
        """ % (title_string)

        for run in range(0, number_runs):
            intro_command = "echo Starting run;export PATH=$PATH:/usr/java/latest/bin; cd %s;" % (clojush_directory)
            outro_command = " > %s%s%i%s; echo Finished Run" % (output_directory, output_prefix, run, output_postfix)

            full_command = intro_command + command + outro_command

            alfcode += """    Task -title {%s - run %i} -cmds {
                RemoteCmd {/bin/sh -c {%s}} -service {%s}
            }
        """ % (title_string, run, full_command, service_tag)

        alfcode += "}\n"

        alf_f.writelines(alfcode)
        alf_f.close()

        # Run tractor command
        source_string = "source /etc/sysconfig/pixar"
        pixar_string = "/opt/pixar/tractor-blade-1.7.2/python/bin/python2.6 /opt/pixar/tractor-blade-1.7.2/tractor-spool.py --engine=fly:8000"

        os.system("%s;%s %s" % (source_string, pixar_string, alf_file_string))
        time.sleep(300)

