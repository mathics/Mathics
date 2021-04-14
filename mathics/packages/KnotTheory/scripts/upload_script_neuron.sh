# kill any previous running MathKernel's (this is obviously kinda dangerous!)
kill `ps -C MathKernel -o pid=`
# start the zombie, appropriately nice'd
./upload_script_nice.sh ScottDataZombie $1 ScottDataRobot_Queue /scratch/bin2/
# ./upload_script_nice.sh ScottQuantumInvariantRobot $1 QuantumInvariants_Queue /scratch/bin2/

