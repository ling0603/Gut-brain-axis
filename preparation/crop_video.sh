#!/bin/bash
#PBS -N crop_video
#PBS -l walltime=02:00:00
#PBS -l select=1:ncpus=1:mem=2gb

module load FFmpeg/7.1.2-GCCcore-14.3.0

VIDEO_DIR="/rds/general/user/xz2822/home/hpc_upload"
OUT_DIR="/rds/general/user/xz2822/home/crop_video"

mkdir -p "$OUT_DIR"

for video in "$VIDEO_DIR"/*.[Mm][Pp]4; do
  base=$(basename "${video%.*}")

  ffmpeg -y \
    -ss 00:03:00 \
    -i "$video" \
    -t 00:24:00 \
    -c copy \
    "$OUT_DIR/${base}_crop.MP4"
done