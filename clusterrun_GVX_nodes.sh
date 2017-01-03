
#!/bin/bash
#$ -N chinaVXnode
#$ -cwd -V
#$ -q short.q
#$ -l mem_free=3G,h_vmem=3.2G
#$ -M rebecca.harris@lshtm.ac.uk -m eas
#$ -t 1-30
R CMD BATCH /home/lsh355020/China_VX/run_model_final_nodes.R out_china_VX_${SGE_TASK_ID}.out

