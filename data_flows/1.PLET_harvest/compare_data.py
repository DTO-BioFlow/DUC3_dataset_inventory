import os
import json
from datetime import datetime

import pandas as pd

CSV_PATH = "output/assessment_data_20260414-1015.csv"
PARQUET_URL = "https://minio.dive.edito.eu/oidc-willemboone/PLET/assessment_data.parquet"
OUTPUT_DIR = "output"


def filter_positive_samples(df: pd.DataFrame) -> pd.DataFrame:
    if "numSamples" not in df.columns:
        raise KeyError("Expected column 'numSamples' is missing.")
    return df[df["numSamples"] > 0].copy()


def build_comparison_frame(df: pd.DataFrame, columns: list[str]) -> pd.DataFrame:
    # Normalize values to stable strings so joins are robust across type differences.
    out = df[columns].copy()
    for col in columns:
        out[col] = out[col].fillna("").astype(str).str.strip()
    return out


def keep_month_periods(df: pd.DataFrame) -> pd.DataFrame:
    period_as_text = df["period"].astype(str).str.strip()
    valid_month_mask = period_as_text.str.fullmatch(r"\d{4}-\d{2}")
    dropped = (~valid_month_mask).sum()
    if dropped:
        invalid_examples = sorted(period_as_text[~valid_month_mask].unique().tolist())[:5]
        print(
            f"Dropping {dropped} changed rows with non-YYYY-MM period values. "
            f"Examples: {invalid_examples}"
        )
    out = df.loc[valid_month_mask].copy()
    out["period"] = period_as_text[valid_month_mask]
    return out


def main() -> None:
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    timestamp = datetime.now().strftime("%Y%m%d-%H%M")

    print("Reading local CSV file...")
    csv_df = pd.read_csv(CSV_PATH)
    csv_filtered = filter_positive_samples(csv_df).reset_index(drop=True)
    print(f"CSV total records: {len(csv_df)}")
    print(f"CSV records with numSamples > 0: {len(csv_filtered)}")

    required_cols = ["dataset_name", "period"]
    missing_required = [col for col in required_cols if col not in csv_filtered.columns]
    if missing_required:
        raise KeyError(
            f"Missing required columns in CSV for change tracking: {missing_required}"
        )

    print("\nReading remote parquet file...")
    try:
        parquet_df = pd.read_parquet(PARQUET_URL)
        parquet_filtered = filter_positive_samples(parquet_df).reset_index(drop=True)
        print(f"Parquet total records: {len(parquet_df)}")
        print(f"Parquet records with numSamples > 0: {len(parquet_filtered)}")

        common_cols = [col for col in csv_filtered.columns if col in parquet_filtered.columns]
        if not common_cols:
            raise ValueError("No common columns found between CSV and parquet datasets.")

        csv_cmp = build_comparison_frame(csv_filtered, common_cols)
        parquet_cmp = build_comparison_frame(parquet_filtered, common_cols)

        merged_new = csv_cmp.merge(
            parquet_cmp.drop_duplicates(),
            on=common_cols,
            how="left",
            indicator=True,
        )

        new_row_mask = (merged_new["_merge"] == "left_only").to_numpy()
        new_records = csv_filtered.loc[new_row_mask].copy()

        merged_removed = parquet_cmp.merge(
            csv_cmp.drop_duplicates(),
            on=common_cols,
            how="left",
            indicator=True,
        )

        removed_row_mask = (merged_removed["_merge"] == "left_only").to_numpy()
        removed_records = parquet_filtered.loc[removed_row_mask].copy()

        new_dataset_periods = (
            new_records[["dataset_name", "period"]]
            .drop_duplicates()
            .sort_values(["dataset_name", "period"])
        )
        removed_dataset_periods = (
            removed_records[["dataset_name", "period"]]
            .drop_duplicates()
            .sort_values(["dataset_name", "period"])
        )

        new_dataset_periods = keep_month_periods(new_dataset_periods)
        removed_dataset_periods = keep_month_periods(removed_dataset_periods)

        new_dataset_periods["changes"] = "NEW"
        removed_dataset_periods["changes"] = "REMOVED"

        changed_dataset_periods = pd.concat(
            [new_dataset_periods, removed_dataset_periods],
            ignore_index=True,
        )

        changed_datasets = (
            changed_dataset_periods.groupby(["changes", "dataset_name"], as_index=False)[
                "period"
            ]
            .agg(lambda values: sorted(values.astype(str).unique().tolist()))
            .rename(columns={"period": "periods"})
        )
        sort_order = {"NEW": 0, "REMOVED": 1}
        changed_datasets = changed_datasets.sort_values(
            by=["changes", "dataset_name"],
            key=lambda col: col.map(sort_order) if col.name == "changes" else col,
        )
        changed_datasets["periods"] = changed_datasets["periods"].apply(json.dumps)

        changes_path = os.path.join(OUTPUT_DIR, f"changed_dataset_periods_{timestamp}.csv")

        changed_datasets.to_csv(changes_path, index=False)

        print("\n" + "=" * 60)
        print("DELTA RESULTS")
        print("=" * 60)
        print(f"NEW dataset_name + period combinations: {len(new_dataset_periods)}")
        print(f"REMOVED dataset_name + period combinations: {len(removed_dataset_periods)}")
        print(f"Total changed dataset_name + period combinations: {len(changed_dataset_periods)}")
        print(f"Changed datasets: {len(changed_datasets)}")
        print(f"Saved changed dataset-period list to: {changes_path}")

    except Exception as error:
        print(f"Error reading/comparing remote parquet: {error}")
        print("No delta CSV was generated because baseline parquet could not be compared.")


if __name__ == "__main__":
    main()
