import pandas as pd
import geopandas as gpd
import requests
from io import BytesIO
from zipfile import ZipFile

DATAGOUV_URL = "https://www.data.gouv.fr/"


def load_table_from_resource_id(resource_id, sep=';', _file_and_format=None, **kwargs):
    """
    Importe une ressource tabulaire dans un pandas.Dataframe
        resource_id: id d'une ressource de data.gouv.fr
        sep: le séparateur du fichier (souvent ';' ou ',')
    """
    if _file_and_format:
        _format = _file_and_format[1]
    else:
        r = requests.get(f"{DATAGOUV_URL}api/2/datasets/resources/{resource_id}")
        r.raise_for_status()
        r = r.json()['resource']
        _format = r["format"]
    if _format == "geojson":
        print("Info : Ce dataframe contient des données géographiques")
        return gpd.read_file(
            f"{DATAGOUV_URL}fr/datasets/r/{resource_id}" if resource_id else _file_and_format[0],
            **kwargs
        )
    elif _format == "zip":
        content = requests.get(f"{DATAGOUV_URL}fr/datasets/r/{resource_id}").content
        the_zip = ZipFile(BytesIO(content))
        files = {f.filename: f.file_size for f in the_zip.filelist}
        largest = max(files, key=files.get)
        _format = ".".join(largest.split(".")[1:])
        print(
            "Info : ce fichier zippé contient plusieurs fichiers, "
            f"ouverture du plus large : {largest}")
        return load_table_from_resource_id(
            None,
            _file_and_format=(the_zip.open(largest), _format),
            sep=sep,
            **kwargs
        )
    elif _format == "parquet":
        return pd.read_parquet(
            f"{DATAGOUV_URL}fr/datasets/r/{resource_id}",
            **kwargs,
        )
    elif _format in ["xls", "xlsx"]:
        return pd.read_excel(
            f"{DATAGOUV_URL}fr/datasets/r/{resource_id}",
            **kwargs,
        )
    return pd.read_csv(
        f"{DATAGOUV_URL}fr/datasets/r/{resource_id}" if resource_id else _file_and_format[0],
        sep=sep,
        compression="gzip" if _format == "csv.gz" else "infer",
        **kwargs,
    )
