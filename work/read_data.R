# dataパスを設定
data_path <- "/home/q/training_task/100knocks-preprocess/docker/work/data"
# csvファイルを読み込む
df_category <- read.table(paste(data_path, "category.csv", sep = "/"),
    header = T,
    sep = ",", fileEncoding = "UTF-8"
)
df_customer <- read.table(paste(data_path, "customer.csv", sep = "/"),
    header = T,
    sep = ",", fileEncoding = "UTF-8"
)
df_geocode <- read.table(paste(data_path, "geocode.csv", sep = "/"),
    header = T,
    sep = ",", fileEncoding = "UTF-8"
)
df_product <- read.table(paste(data_path, "product.csv", sep = "/"),
    header = T,
    sep = ",", fileEncoding = "UTF-8"
)
df_receipt <- read.table(paste(data_path, "receipt.csv", sep = "/"),
    header = T,
    sep = ",", fileEncoding = "UTF-8"
)
df_store <- read.table(paste(data_path, "store.csv", sep = "/"),
    header = T,
    sep = ",", fileEncoding = "UTF-8"
)
dim(df_category)
dim(df_customer)
dim(df_geocode)
dim(df_product)
dim(df_receipt)
dim(df_store)
