# QUES-C - AKSARA version
Quantification of Environmental Services (QUES) - Carbon through AKSARA

## Usage
```
Usage: run.R [options]


Options:
        -w CHARACTER, --workdir=CHARACTER
                set working directory

        -d CHARACTER, --admin=CHARACTER
                administrative boundary

        -i CHARACTER, --landcover1=CHARACTER
                initial land-cover

        -b CHARACTER, --year1=NUMERIC
                initial period

        -f CHARACTER, --landcover2=CHARACTER
                final land-cover

        -e CHARACTER, --year2=NUMERIC
                final period

        -u CHARACTER, --landcover_table=CHARACTER
                land-cover class

        -z CHARACTER, --zone=CHARACTER
                zone or planning unit

        -k CHARACTER, --zone_table=CHARACTER
                zone class

        -c CHARACTER, --carbon_stock=CHARACTER
                carbon stock

        -p CHARACTER, --peat=CHARACTER
                peat

        -x CHARACTER, --peat_table=CHARACTER
                peat class

        -h CHARACTER, --burn=CHARACTER
                burn

        -o CHARACTER, --output=CHARACTER
                output file name [default=change_map.tif]
```

## Example
```
Rscript run.R 
  -d data/raster_ha/Jawa_Barat/Admin_jabar.tif
  -i data/raster_ha/Jawa_Barat/PL00_jabar.tif
  -b 2000
  -f data/raster_ha/Jawa_Barat/PL00_jabar.tif
  -e 2003
  -u data/tabular/Tabel_acuan_tutupan_lahan.csv
  -z data/raster_ha/Jawa_Barat/Fungsikawasan_jabar.tif
  -k data/tabular/Tabel_acuan_fungsi_kawasan.csv
  -c data/tabular/cstock.csv
  -p data/raster_ha/Jawa_Barat/Peat_jabar.tif
  -x data/tabular/Faktor_emisi_perubahan_gambut.csv
  -h data/raster_ha/Jawa_Barat/Burn18_jabar.tif
```