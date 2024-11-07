#!/bin/bash
#SBATCH --job-name=fitmods
#SBATCH --output=/group/orben/projects/2022_turner_twittermodelling/analyses/proof_of_concept/alspac_gt/02-model_fitting_and_simulation/slurm_outputs/output.txt
#SBATCH --time=100:00:00
#SBATCH --ntasks=7
#SBATCH --cpus-per-task=1

module purge
module load R/4.1.2plus

# define possible model values
model_list=("FP" "CP" "RL1" "RL2" "PH" "RLH1" "RLH2")


# iterate over model values and fit_models for each one.

for model in "${model_list[@]}"; 
do
    echo "Running task with model: ${model}"
    output_file="/group/orben/projects/2022_turner_twittermodelling/analyses/proof_of_concept/alspac_gt/02-model_fitting_and_simulation/slurm_outputs/output_${model}.txt"
    
    # Execute R script in parallel, capturing time and redirecting output and error streams to the same file
    { time srun -N1 -n1 --exclusive Rscript /group/orben/projects/2022_turner_twittermodelling/analyses/proof_of_concept/alspac_gt/02-model_fitting_and_simulation/02-fit_models.R "$model"; } &> "${output_file}" &
done

wait
