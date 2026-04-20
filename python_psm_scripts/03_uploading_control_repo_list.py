import importlib.util
import sys
import pandas as pd
from pathlib import Path
from sqlalchemy import text
from datetime import datetime

def import_module_from_path(module_name: str, file_path: str):
    """Imports a module from a specific file path safely."""
    if not Path(file_path).exists():
        raise FileNotFoundError(f"The file {file_path} does not exist.")

    spec = importlib.util.spec_from_file_location(module_name, Path(file_path))
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module

HELPERS_PATH = "/Users/admin/Nextcloud/08_Accompanying Research/08_08_Data_Extraction/src/helpers.py"

helpers = import_module_from_path("helpers", HELPERS_PATH)

repos_list = helpers.preprocess_repos_list_for_postgres(
    "../data/proc/propensity_match_controls.txt"
)

engine = helpers.create_postgres_engine(location = "local")

repos_list.drop_duplicates(subset = "sha_id", inplace = True)
print(f"Unique repositories after dropping duplicates: {len(repos_list)}")

repos_list["added_at"] = datetime.utcnow()

repos_list.to_sql("temp_repo_names", engine, if_exists="replace", index=False)

insert_sql = """
INSERT INTO repo_names (sha_id, url, owner, repo, host, repo_group_id, added_at)
SELECT
    sha_id,
    url,
    owner,
    repo,
    host,
    204 AS repo_group_id,
    added_at
FROM temp_repo_names
ON CONFLICT (owner, repo)
DO UPDATE SET
    sha_id = EXCLUDED.sha_id,
    url = EXCLUDED.url,
    host = EXCLUDED.host,
    repo_group_id = EXCLUDED.repo_group_id,
    added_at = EXCLUDED.added_at;
    """

with engine.begin() as conn:
    result = conn.execute(text(insert_sql))
    rows_inserted = result.rowcount
    conn.execute(text("DROP TABLE temp_repo_names;"))

print(f"Success! New 204 rows inserted: {rows_inserted}")