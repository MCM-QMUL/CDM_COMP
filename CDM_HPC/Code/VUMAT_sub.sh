#!/bin/bash
#$ -pe smp 1
# -l h_vmem=2G
# -l highmem
#$ -l h_rt=1:0:0
#$ -cwd
#$ -j y
#$ -N abq-job
#$ -l abaqus=5

# Load the required modules
module load abaqus/6.14-2
# module load abaqus/2020
module load intel
#abaqus make library=vumat.f

/bin/echo Running on host: `hostname`.
/bin/echo Starting on: `date`.

# make dir
mkdir /data/scratch/exx530/$JOB_ID
mkdir $SGE_O_WORKDIR/$JOB_ID

# copy command
cp $SGE_O_WORKDIR/* /data/scratch/exx530/$JOB_ID
cd /data/scratch/exx530/$JOB_ID
/bin/echo In directory: `pwd`

# Replace the following line with abaqus command
#abaqus job=callmat cpus=${NSLOTS} user=PhaseField_5m-std.o mp_mode=THREADS scratch=/data/scratch/exx530/

# end of environment file setup

#abaqus job=TPB_honey_Nx96.inp cpus=${NSLOTS} user=PhaseField_call_mat_AT1_D0.for mp_mode=THREADS scratch=/data/scratch/exx530/
abaqus job=Shear.inp cpus=${NSLOTS} user=vumat.for mp_mode=THREADS scratch=/data/scratch/exx530/ memory="90 %" interactive
#sleep 300000

echo Now it is: `date`

# copy back
mkdir $SGE_O_WORKDIR/$JOB_ID
cp /data/scratch/exx530/$JOB_ID/* $SGE_O_WORKDIR/$JOB_ID
#cd /data/scratch/exx530/
#cp /data/scratch/exx530/$JOB_ID/* /data/home/exx530/PFMHT/
# clean up
rm -rf /data/scratch/exx530/$JOB_ID

