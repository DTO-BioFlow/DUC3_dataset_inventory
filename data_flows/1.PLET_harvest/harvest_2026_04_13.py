import os
import s3fs
import tempfile
import xarray as xr
import pandas as pd
from pathlib import Path
from datetime import date
from typing import Optional
from datetime import datetime
from harvest_plet.plet import PLETHarvester
from harvest_plet.harvest_for_assessment import harvest_for_assessment


def _load_env_file(env_path: Path) -> None:
    if not env_path.exists():
        return

    for raw_line in env_path.read_text(encoding="utf-8").splitlines():
        line = raw_line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue

        key, value = line.split("=", 1)
        key = key.strip()
        value = value.strip().strip('"').strip("'")
        os.environ.setdefault(key, value)


def _load_s3_settings_from_env() -> dict:
    env_path = Path(__file__).resolve().parent / ".env"
    _load_env_file(env_path)

    # Support both explicit S3_* names and lowercase names.
    return {
        "bucket": os.getenv("S3_BUCKET") or os.getenv("bucket"),
        "endpoint_url": os.getenv("S3_ENDPOINT_URL") or os.getenv("endpoint_url"),
        "aws_access_key_id": os.getenv("AWS_ACCESS_KEY_ID") or os.getenv("aws_access_key_id"),
        "aws_secret_access_key": os.getenv("AWS_SECRET_ACCESS_KEY") or os.getenv("aws_secret_access_key"),
        "aws_session_token": os.getenv("AWS_SESSION_TOKEN") or os.getenv("aws_session_token"),
    }


def _validate_required_env_settings(settings: dict) -> None:
    required = [
        "bucket",
        "endpoint_url",
        "aws_access_key_id",
        "aws_secret_access_key",
    ]
    missing = [key for key in required if not settings.get(key)]

    if missing:
        env_names = {
            "bucket": "S3_BUCKET",
            "endpoint_url": "S3_ENDPOINT_URL",
            "aws_access_key_id": "AWS_ACCESS_KEY_ID",
            "aws_secret_access_key": "AWS_SECRET_ACCESS_KEY",
        }
        missing_env = [env_names[key] for key in missing]
        raise ValueError(
            "Missing required .env variables: " + ", ".join(missing_env)
        )


def _sanitize_dataframe(df: pd.DataFrame) -> pd.DataFrame:
    df = df.copy()

    if "num_samples" in df.columns:
        df = df[df["num_samples"] != 0]

    df.columns = [
        c.replace("/", "_").replace(" ", "_").replace("\\", "_")
        for c in df.columns
    ]
    return df


def _validate_s3(use_s3, bucket, endpoint_url, key, secret):
    if use_s3 and not all([bucket, endpoint_url, key, secret]):
        raise ValueError("Missing S3 credentials or bucket configuration.")


def export_to_csv(
    df: pd.DataFrame,
    out_path: str = "merged.csv",
    use_s3: bool = False,
    bucket: Optional[str] = None,
    endpoint_url: Optional[str] = None,
    aws_access_key_id: Optional[str] = None,
    aws_secret_access_key: Optional[str] = None,
    aws_session_token: Optional[str] = None,
) -> None:
    df = _sanitize_dataframe(df)
    _validate_s3(use_s3, bucket, endpoint_url,
                 aws_access_key_id, aws_secret_access_key)

    if use_s3:
        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".csv", delete=False, encoding="utf-8"
        ) as tmp:
            df.to_csv(tmp.name, index=False)
            tmp_path = tmp.name

        fs = s3fs.S3FileSystem(
            key=aws_access_key_id,
            secret=aws_secret_access_key,
            token=aws_session_token,
            client_kwargs={"endpoint_url": endpoint_url},
        )

        s3_key = f"{bucket}/{out_path.lstrip('/')}"
        fs.put(tmp_path, s3_key)
        Path(tmp_path).unlink()

        print(f"CSV uploaded to s3://{s3_key}")

    else:
        out_file = Path(out_path)
        out_file.parent.mkdir(parents=True, exist_ok=True)
        df.to_csv(out_file, index=False)
        print(f"CSV saved locally at {out_file}")


def to_parquet(
    df: pd.DataFrame,
    out_path: str = "merged-data.parquet",
    use_s3: bool = False,
    bucket: Optional[str] = None,
    endpoint_url: Optional[str] = None,
    aws_access_key_id: Optional[str] = None,
    aws_secret_access_key: Optional[str] = None,
    aws_session_token: Optional[str] = None,
) -> None:
    df = _sanitize_dataframe(df)
    _validate_s3(use_s3, bucket, endpoint_url,
                 aws_access_key_id, aws_secret_access_key)

    if use_s3:
        storage_options = {
            "key": aws_access_key_id,
            "secret": aws_secret_access_key,
            "token": aws_session_token,
            "client_kwargs": {"endpoint_url": endpoint_url},
        }

        s3_path = f"s3://{bucket}/{out_path.lstrip('/')}"
        df.to_parquet(
            s3_path,
            index=False,
            engine="pyarrow",
            storage_options=storage_options,
        )
        print(f"Parquet uploaded to {s3_path}")

    else:
        out_file = Path(out_path)
        out_file.parent.mkdir(parents=True, exist_ok=True)
        df.to_parquet(out_file, index=False, engine="pyarrow")
        print(f"Parquet saved locally at {out_file}")


def to_netcdf(
    df: pd.DataFrame,
    out_path: str = "merged-data.nc",
    use_s3: bool = False,
    bucket: Optional[str] = None,
    endpoint_url: Optional[str] = None,
    aws_access_key_id: Optional[str] = None,
    aws_secret_access_key: Optional[str] = None,
    aws_session_token: Optional[str] = None,
) -> None:
    df = _sanitize_dataframe(df)
    ds = xr.Dataset.from_dataframe(df)

    _validate_s3(use_s3, bucket, endpoint_url,
                 aws_access_key_id, aws_secret_access_key)

    if use_s3:
        tmp_dir = Path(".cache_merged")
        tmp_dir.mkdir(exist_ok=True)

        local_file = tmp_dir / out_path
        local_file.parent.mkdir(parents=True, exist_ok=True)

        ds.to_netcdf(local_file, engine="h5netcdf")

        fs = s3fs.S3FileSystem(
            key=aws_access_key_id,
            secret=aws_secret_access_key,
            token=aws_session_token,
            client_kwargs={"endpoint_url": endpoint_url},
        )

        s3_key = f"{bucket}/{out_path.lstrip('/')}"
        fs.put(str(local_file), s3_key)
        local_file.unlink()

        print(f"NetCDF uploaded to s3://{s3_key}")

    else:
        out_file = Path(out_path)
        out_file.parent.mkdir(parents=True, exist_ok=True)
        ds.to_netcdf(out_file, engine="h5netcdf")
        print(f"NetCDF saved locally at {out_file}")


if __name__ == "__main__":
    s3_settings = _load_s3_settings_from_env()
    _validate_required_env_settings(s3_settings)

    start_date = date(2015, 1, 1)
    end_date = date(2025, 1, 1)

    plet_harvester = PLETHarvester()
    plet_harvester.set_instance("PLET-DOME")



    df = harvest_for_assessment(start_date=start_date,
                                end_date=end_date,
                                plet_harvester=plet_harvester
                                )


    for i, item in enumerate(df.head(10).itertuples()):
        print(i, item)

    # # Export merged Parquet directly to MinIO/S3
    to_parquet(
        df=df,
        out_path="PLET/assessment_data_PLET_DOME.parquet",
        use_s3=True,
            bucket=s3_settings["bucket"],
            endpoint_url=s3_settings["endpoint_url"],
            aws_access_key_id=s3_settings["aws_access_key_id"],
            aws_secret_access_key=s3_settings["aws_secret_access_key"],
            aws_session_token=s3_settings["aws_session_token"],
    )

    # to_netcdf(
    #     df=df,
    #     out_path="PLET/assessment_data.nc",
    #     use_s3=True,
    #     bucket=s3_settings["bucket"],
    #     endpoint_url=s3_settings["endpoint_url"],
    #     aws_access_key_id=s3_settings["aws_access_key_id"],
    #     aws_secret_access_key=s3_settings["aws_secret_access_key"],
    #     aws_session_token=s3_settings["aws_session_token"],
    # )

    # now = datetime.now().strftime("%Y%m%d-%H%M")
    #
    # export_to_csv(
    #     df=df,
    #     out_path=f"output/assessment_data_{now}.csv",
    #     use_s3=False,
    #     bucket=s3_settings["bucket"],
    #     endpoint_url=s3_settings["endpoint_url"],
    #     aws_access_key_id=s3_settings["aws_access_key_id"],
    #     aws_secret_access_key=s3_settings["aws_secret_access_key"],
    #     aws_session_token=s3_settings["aws_session_token"],
    # )