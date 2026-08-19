#!/usr/bin/env python3
"""
Run the trained bee-tag model on multiple videos in parallel within ONE PBS job.

Eight-video production run:
- N_WORKERS = 8
- each worker processes one video at a time
- each worker loads its own copy of the ResNet model
- each worker uses CPU only
- the first eight matched videos are processed in parallel

This imports the already-tested one-video workflow from bee_tag_model_worker.py.
"""

from __future__ import annotations

# Set numerical thread limits before importing NumPy, OpenCV, or PyTorch.
import os

CPU_THREADS_PER_WORKER = int(
    os.environ.get("CPU_THREADS_PER_WORKER", "8")
)

os.environ["OMP_NUM_THREADS"] = str(CPU_THREADS_PER_WORKER)
os.environ["MKL_NUM_THREADS"] = str(CPU_THREADS_PER_WORKER)
os.environ["OPENBLAS_NUM_THREADS"] = str(CPU_THREADS_PER_WORKER)
os.environ["NUMEXPR_NUM_THREADS"] = str(CPU_THREADS_PER_WORKER)

from concurrent.futures import ProcessPoolExecutor, as_completed
from pathlib import Path
import argparse
import multiprocessing as mp
import sys
import time
import traceback


VIDEO_EXTENSIONS = {".mp4", ".mov", ".avi", ".mkv", ".m4v"}

EXCLUDED_CSV_TERMS = (
    "final",
    "crop_index",
    "with_ocr",
    "with_head_tail",
    "with_tag_crop",
    "with_thorax_crop",
    "tag_model_predictions",
    "with_model",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()

    parser.add_argument("--working-dir", required=True, type=Path)
    parser.add_argument("--model", required=True, type=Path)
    parser.add_argument("--output-root", required=True, type=Path)

    parser.add_argument("--workers", type=int, default=8)
    parser.add_argument(
        "--limit-videos",
        type=int,
        default=8,
        help="Process the first eight alphabetically matched videos.",
    )

    parser.add_argument("--frame-interval", type=int, default=30)
    parser.add_argument("--video-crop-size", type=int, default=100)
    parser.add_argument("--likelihood-threshold", type=float, default=0.6)
    parser.add_argument("--confidence-threshold", type=float, default=0.5)
    parser.add_argument("--batch-size", type=int, default=64)
    parser.add_argument("--max-frames", type=int, default=None)
    parser.add_argument("--save-crops", action="store_true")
    parser.add_argument("--overwrite", action="store_true")

    args = parser.parse_args()

    if args.workers < 1:
        parser.error("--workers must be at least 1")

    if args.limit_videos is not None and args.limit_videos < 1:
        parser.error("--limit-videos must be at least 1")

    for path, label in (
        (args.working_dir, "working directory"),
        (args.model, "model checkpoint"),
    ):
        if not path.exists():
            parser.error(f"{label} does not exist: {path}")

    return args


def find_pairs(
    working_dir: Path,
) -> list[tuple[Path, Path]]:
    videos = sorted(
        path
        for path in working_dir.iterdir()
        if path.is_file()
        and path.suffix.lower() in VIDEO_EXTENSIONS
    )

    pairs: list[tuple[Path, Path]] = []

    for video in videos:
        candidates = sorted(
            path
            for path in working_dir.glob(f"{video.stem}*.csv")
            if not any(
                term in path.name.lower()
                for term in EXCLUDED_CSV_TERMS
            )
        )

        # Prefer a raw DeepLabCut CSV.
        dlc_candidates = [
            path
            for path in candidates
            if "dlc" in path.name.lower()
        ]
        candidates = dlc_candidates or candidates

        if not candidates:
            print(
                f"WARNING: no matching raw DLC CSV for {video.name}",
                flush=True,
            )
            continue

        if len(candidates) > 1:
            print(
                f"WARNING: multiple CSVs matched {video.name}; "
                f"using {candidates[0].name}",
                flush=True,
            )

        pairs.append((video, candidates[0]))

    return pairs


def process_pair(
    video_text: str,
    csv_text: str,
    model_text: str,
    output_text: str,
    frame_interval: int,
    video_crop_size: int,
    likelihood_threshold: float,
    confidence_threshold: float,
    batch_size: int,
    max_frames: int | None,
    save_crops: bool,
    overwrite: bool,
) -> dict[str, str | float]:
    """
    Worker entry point.

    Each process imports PyTorch independently, loads one model copy, and
    processes one video using CPU.
    """
    worker_start = time.time()

    import cv2
    import torch
    from PIL import Image

    import bee_tag_model_worker as worker

    cv2.setNumThreads(max(1, CPU_THREADS_PER_WORKER))
    torch.set_num_threads(max(1, CPU_THREADS_PER_WORKER))

    try:
        torch.set_num_interop_threads(1)
    except RuntimeError:
        pass

    video_path = Path(video_text)
    csv_path = Path(csv_text)
    model_path = Path(model_text)
    output_root = Path(output_text)

    # Configure the imported tested workflow.
    worker.OUTPUT_ROOT = output_root
    worker.MODEL_PATH = model_path
    worker.VIDEO_CROP_SIZE = video_crop_size
    worker.LIKELIHOOD_THRESHOLD = likelihood_threshold
    worker.MAX_FRAMES = max_frames
    worker.FRAME_INTERVAL = frame_interval
    worker.BATCH_SIZE = batch_size
    worker.CONFIDENCE_THRESHOLD = confidence_threshold
    worker.DEVICE = "cpu"
    worker.SAVE_CROPS = save_crops
    worker.SKIP_EXISTING_OUTPUTS = not overwrite

    print(
        f"[{video_path.name}] Worker PID={os.getpid()}, "
        f"CPU threads={CPU_THREADS_PER_WORKER}",
        flush=True,
    )

    device = worker.choose_device("cpu")

    (
        model,
        class_names,
        model_image_size,
        architecture,
        checkpoint,
    ) = worker.load_trained_model(
        model_path=model_path,
        device=device,
    )

    transform = worker.build_model_transform(
        model_image_size=model_image_size,
    )

    final_csv = worker.process_one_video(
        video_path=video_path,
        csv_path=csv_path,
        model=model,
        class_names=class_names,
        transform=transform,
        device=device,
    )

    elapsed = time.time() - worker_start

    return {
        "video": video_path.name,
        "status": "completed" if final_csv else "no_output",
        "final_csv": str(final_csv or ""),
        "elapsed_seconds": elapsed,
        "error": "",
    }


def main() -> None:
    args = parse_args()
    args.output_root.mkdir(parents=True, exist_ok=True)

    pairs = find_pairs(args.working_dir)

    if not pairs:
        raise SystemExit("No matched video/raw-DLC-CSV pairs were found.")

    if args.limit_videos is not None:
        pairs = pairs[: args.limit_videos]

    workers = min(args.workers, len(pairs))

    print("=" * 80)
    print("INTERNAL CPU PARALLEL RUN")
    print("=" * 80)
    print(f"Matched pairs selected:   {len(pairs)}")
    print(f"Worker processes:         {workers}")
    print(f"CPU threads per worker:   {CPU_THREADS_PER_WORKER}")
    print(f"Expected total CPU cores: {workers * CPU_THREADS_PER_WORKER}")
    print()

    for number, (video, csv_path) in enumerate(pairs, start=1):
        print(f"{number}. {video.name}")
        print(f"   {csv_path.name}")

    results: list[dict[str, str | float]] = []

    # spawn is safer than fork for PyTorch/OpenCV worker processes.
    context = mp.get_context("spawn")

    with ProcessPoolExecutor(
        max_workers=workers,
        mp_context=context,
    ) as executor:
        futures = {
            executor.submit(
                process_pair,
                str(video),
                str(csv_path),
                str(args.model),
                str(args.output_root),
                args.frame_interval,
                args.video_crop_size,
                args.likelihood_threshold,
                args.confidence_threshold,
                args.batch_size,
                args.max_frames,
                args.save_crops,
                args.overwrite,
            ): video
            for video, csv_path in pairs
        }

        for future in as_completed(futures):
            video = futures[future]

            try:
                result = future.result()
                results.append(result)
                print(
                    f"COMPLETED: {video.name} in "
                    f"{float(result['elapsed_seconds']):.1f} seconds",
                    flush=True,
                )

            except Exception as error:
                traceback.print_exc()
                results.append(
                    {
                        "video": video.name,
                        "status": "failed",
                        "final_csv": "",
                        "elapsed_seconds": 0.0,
                        "error": f"{type(error).__name__}: {error}",
                    }
                )
                print(
                    f"FAILED: {video.name}: {error}",
                    file=sys.stderr,
                    flush=True,
                )

    import pandas as pd

    summary = pd.DataFrame(results)
    summary_path = args.output_root / "parallel_processing_summary.csv"
    summary.to_csv(summary_path, index=False)

    print()
    print("=" * 80)
    print("ALL SELECTED VIDEOS FINISHED")
    print("=" * 80)
    print(summary["status"].value_counts(dropna=False).to_string())
    print(f"Summary: {summary_path}")

    if (summary["status"] == "failed").any():
        raise SystemExit(1)


if __name__ == "__main__":
    main()
