#!/bin/bash
read -p "Please enter the number of runs: " X
if ! [[ "$X" =~ ^[0-9]+$ ]]; then
    echo "Error: '$X' is not a valid number. Exiting..."
    exit 1
fi
echo "--------------------------------------------------"
echo "Notice: You are about to run the script with $X runs."
read -p "Do you want to proceed? (Type 'Y' to confirm, any other key to cancel): " CONFIRM
echo "--------------------------------------------------"
if [[ "$CONFIRM" != "Y" && "$CONFIRM" != "y" ]]; then
    echo "Operation cancelled. No scripts were launched."
    exit 0
fi
echo "Confirmed. Starting $X processes in the background..."
for i in $(seq 1 $X); do
  #nohup Rscript Analysis/N.Speciation.Extinction.r > "../running.log/output_$i.log" 2>&1 &
  nohup Rscript Analysis/species.type.with.bridge.r > "../running.log/output_$i.log" 2>&1 &
done
echo "Successfully launched $X background tasks."

#kill -9 $(pgrep -f "species.type.with.bridge.r")