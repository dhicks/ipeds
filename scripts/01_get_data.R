years = 1984:2016
base_filenames = c(
    paste0('c', 1984:1989, '_cip'),
    'c8990cip',
    paste0('c', 1991:1994, '_cip'),
    paste0('c', 94:98, 95:99, '_a'),
    paste0('c', 2000:2016, '_a')
)

## Download files
urls = paste0('https://nces.ed.gov/ipeds/datacenter/data/', 
              base_filenames, '.zip')

data_folder = '../data'
download_filenames = paste0(data_folder, '/', 
                            base_filenames, '.zip')
download.file(urls, download_filenames)

lapply(download_filenames, unzip, exdir = data_folder)

## Load files
data_files = ifelse(
    file.exists(paste0(data_folder, '/', base_filenames, '_rv.csv')), 
    paste0(data_folder, '/', base_filenames, '_rv.csv'), 
    paste0(data_folder, '/', base_filenames, '.csv')
)

datafs = lapply(data_files, readr::read_csv)

names(datafs) = years
datafs = lapply(datafs, function (df) {
    names(df) = tolower(names(df))
    df
})

## Select variables of interest
datafs_sel = lapply(datafs, dplyr::select, 'unitid', 
                'cipcode', 'awlevel',
              starts_with('crace15'), starts_with('ctotalm'), 
              starts_with('crace16'), starts_with('ctotalw'))

ipeds_1984_2016 = data.table::rbindlist(datafs_sel, 
                              use.names = TRUE, fill = TRUE, 
                              idcol = 'year')

## Combine women and men totals
ipeds_1984_2016$total_m = with(ipeds_1984_2016, 
                               ifelse(!is.na(crace15), 
                                      crace15, 
                                      ctotalm))
ipeds_1984_2016$total_w = with(ipeds_1984_2016, 
                               ifelse(!is.na(crace16), 
                                      crace16, 
                                      ctotalw))

ipeds_1984_2016 = dplyr::select(ipeds_1984_2016, 
                                year, unitid, cipcode, awlevel, 
                                total_w, total_m)

save(ipeds_1984_2016, file = paste0(data_folder, '01-ipeds_1984_2016.Rdata'))
