#*===============================================*
# データサイエンス 100本ノック（P-001～P-050）
#*===============================================*
library(tidyverse) 

file_path <- "G:/マイドライブ/data4Cs/研修/202111_DS-100knocks/output/"

## P-001 ##
 # レシート明細のデータフレーム（df_receipt）から全項目の先頭10件を表示し、
 # どのようなデータを保有しているか目視で確認せよ。;
df_receipt[1:10,]
head(df_receipt,10)


## P-002 ##
# レシート明細のデータフレーム（df_receipt）から売上日（sales_ymd）、顧客ID（customer_id）、
# 商品コード（product_cd）、売上金額（amount）の順に列を指定し、10件表示させよ。;
df_receipt[1:10,c("sales_ymd","customer_id","product_cd","amount")]


## P-003 ##
# レシート明細のデータフレーム（df_receipt）から売上日（sales_ymd）、顧客ID（customer_id）、
# 商品コード（product_cd）、売上金額（amount）の順に列を指定し、10件表示させよ。
# ただし、sales_ymdはsales_dateに項目名を変更しながら抽出すること。;
df_receipt[1:10,] %>%
  select(sales_date=sales_ymd,customer_id, product_cd, amount)


## P-004 ##
# レシート明細のデータフレーム（df_receipt）から売上日（sales_ymd）、顧客ID（customer_id）、
# 商品コード（product_cd）、売上金額（amount）の順に列を指定し、以下の条件を満たすデータを抽出せよ。
#   ・顧客ID（customer_id）が"CS018205000001";
df_receipt %>%
  filter(customer_id=="CS018205000001") %>%
  select(sales_ymd,customer_id, product_cd, amount)


## P-005 ##
# レシート明細のデータフレーム（df_receipt）から売上日（sales_ymd）、顧客ID（customer_id）、
# 商品コード（product_cd）、売上金額（amount）の順に列を指定し、以下の条件を満たすデータを抽出せよ。
#   ・顧客ID（customer_id）が"CS018205000001"
#   ・売上金額（amount）が1,000以上;
df_receipt %>%
  filter(customer_id=="CS018205000001", amount >= 1000) %>%
  select(sales_ymd,customer_id, product_cd, amount)


## P-006 ##
# レシート明細のデータフレーム（df_receipt）から売上日（sales_ymd）、顧客ID（customer_id）、
# 商品コード（product_cd）、売上数量（quantity）、売上金額（amount）の順に列を指定し、
# 以下の条件を満たすデータを抽出せよ。
#   ・顧客ID（customer_id）が"CS018205000001"
#   ・売上金額（amount）が1,000以上または売上数量（quantity）が5以上
df_receipt %>%
  filter(customer_id=="CS018205000001", (amount >= 1000 | quantity >= 5)) %>%
  select(sales_ymd,customer_id, product_cd, quantity, amount)


## P-007 ##
# レシート明細のデータフレーム（df_receipt）から売上日（sales_ymd）、顧客ID（customer_id）、
# 商品コード（product_cd）、売上金額（amount）の順に列を指定し、以下の条件を満たすデータを抽出せよ。
#   ・顧客ID（customer_id）が"CS018205000001"
#   ・売上金額（amount）が1,000以上2,000以下
df_receipt %>%
  filter(customer_id=="CS018205000001", amount >= 1000, amount <= 2000) %>%
  select(sales_ymd,customer_id, product_cd, amount)


## P-008 ##
# レシート明細のデータフレーム（df_receipt）から売上日（sales_ymd）、顧客ID（customer_id）、
# 商品コード（product_cd）、売上金額（amount）の順に列を指定し、以下の条件を満たすデータを抽出せよ。
#   ・顧客ID（customer_id）が"CS018205000001"
#   ・商品コード（product_cd）が"P071401019"以外
df_receipt %>%
  filter(customer_id=="CS018205000001", product_cd != "P071401019") %>%
  select(sales_ymd,customer_id, product_cd, amount) %>%
  arrange(product_cd, sales_ymd)


## P-009 ##
# 以下の処理において、出力結果を変えずにORをANDに書き換えよ。
filter(df_store, !(prefecture_cd == "13" | floor_area >  900))
filter(df_store,   prefecture_cd != "13",  floor_area <= 900 )


## P-010 ##
# 店舗データフレーム（df_store）から、店舗コード（store_cd）が"S14"で始まるものだけ全項目抽出し、
# 10件だけ表示せよ。
df_store[substr(df_store$store_cd,1,3) == "S14",] %>% head(10)


## P-011 ##
# 顧客データフレーム（df_customer）から顧客ID（customer_id）の末尾が1のものだけ全項目抽出し、
# 10件だけ表示せよ。
df_store[grepl("1$",df_store$store_cd),] %>% head(10)


## P-012 ##
# 店舗データフレーム（df_store）から横浜市の店舗だけ全項目表示せよ。
df_store[grepl("横浜市",df_store$address),] %>% head(10)


## P-013 ##
# 顧客データフレーム（df_customer）から、ステータスコード（status_cd）の先頭が
# アルファベットのA〜Fで始まるデータを全項目抽出し、10件だけ表示せよ。
df_customer[grepl("^[ABCDEF]",df_customer$status_cd),] %>% head(10)


## P-014 ##
# 顧客データフレーム（df_customer）から、ステータスコード（status_cd）の末尾が
# 数字の1〜9で終わるデータを全項目抽出し、10件だけ表示せよ。
df_customer[grepl("[123456789]$",df_customer$status_cd),] %>% head(10)


## P-015 ##
# 顧客データフレーム（df_customer）から、ステータスコード（status_cd）の先頭が
# アルファベットのA〜Fで始まり、末尾が数字の1〜9で終わるデータを全項目抽出し、10件だけ表示せよ。
df_customer[grepl("^[ABCDEF].*[123456789]$",df_customer$status_cd),] %>% head(10)


## P-016 ##
# 店舗データフレーム（df_store）から、電話番号（tel_no）が3桁-3桁-4桁のデータを全項目表示せよ。
df_store[grepl("...-...-....",df_store$tel_no), "tel_no"] %>% head(10)


## P-017 ##
# 顧客データフレーム（df_customer）を生年月日（birth_day）で高齢順にソートし、先頭10件を全項目表示せよ。
df_customer %>%
  dplyr::arrange(birth_day) %>%
  head(10)


## P-018 ##
# 顧客データフレーム（df_customer）を生年月日（birth_day）で若い順にソートし、先頭10件を全項目表示せよ。
df_customer %>%
  dplyr::arrange(desc(birth_day)) %>%
  head(10)


## P-019 ##
# レシート明細データフレーム（df_receipt）に対し、1件あたりの売上金額（amount）が高い順にランクを付与し、
# 先頭10件を抽出せよ。項目は顧客ID（customer_id）、売上金額（amount）、付与したランクを表示させること。
# なお、売上金額（amount）が等しい場合は同一順位を付与するものとする。
df_receipt %>%
  arrange(desc(amount)) %>%
  mutate(R = min_rank(desc(amount))) %>%
  select(customer_id, amount, R) %>%
  head(10)


## P-020 ##
# レシート明細データフレーム（df_receipt）に対し、1件あたりの売上金額（amount）が高い順にランクを付与し、
# 先頭10件を抽出せよ。項目は顧客ID（customer_id）、売上金額（amount）、付与したランクを表示させること。
# なお、売上金額（amount）が等しい場合でも別順位を付与すること。
df_receipt %>%
  arrange(desc(amount)) %>%
  mutate(R = row_number(desc(amount))) %>%
  select(customer_id, amount, R) %>%
  head(10)


## P-021 ##
# レシート明細データフレーム（df_receipt）に対し、件数をカウントせよ。
nrow(df_receipt)


## P-022 ##
# レシート明細データフレーム（df_receipt）の顧客ID（customer_id）に対し、ユニーク件数をカウントせよ。
nrow(dplyr::distinct(df_receipt,customer_id))


## P-023 ##
# レシート明細データフレーム（df_receipt）に対し、店舗コード（store_cd）ごとに売上金額（amount）と
# 売上数量（quantity）を合計せよ。
df_receipt %>%
  group_by(store_cd) %>%
  summarise(amount_sum   = sum(amount),
            quantity_sum = sum(quantity))


## P-024 ##
# レシート明細データフレーム（df_receipt）に対し、顧客ID（customer_id）ごとに
# 最も新しい売上日（sales_ymd）を求め、10件表示せよ。
df_receipt %>%
  group_by(customer_id) %>%
  summarise(sales_ymd = max(sales_ymd)) %>%
  head(10)


## P-025 ##
# レシート明細データフレーム（df_receipt）に対し、顧客ID（customer_id）ごとに
# 最も古い売上日（sales_ymd）を求め、10件表示せよ。
df_receipt %>%
  group_by(customer_id) %>%
  summarise(sales_ymd = min(sales_ymd)) %>%
  head(10)


## P-026 ##
# レシート明細データフレーム（df_receipt）に対し、顧客ID（customer_id）ごとに
# 最も新しい売上日（sales_ymd）と古い売上日を求め、両者が異なるデータを10件表示せよ。
df_receipt %>%
  group_by(customer_id) %>%
  summarise(min = min(sales_ymd),
            max = max(sales_ymd)) %>%
  filter(min != max) %>%
  head(10)


## P-027 ##
# レシート明細データフレーム（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の平均を計算し、降順でTOP5を表示せよ。
df_receipt %>%
  group_by(store_cd) %>%
  summarise(amount_mean = mean(amount)) %>%
  arrange(desc(amount_mean)) %>%
  head(5)


## P-028 ##
# レシート明細データフレーム（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の中央値を計算し、降順でTOP5を表示せよ。
df_receipt %>%
  group_by(store_cd) %>%
  summarise(amount_median = median(amount)) %>%
  arrange(desc(amount_median)) %>%
  head(5)


## P-029 ##
# レシート明細データフレーム（df_receipt）に対し、店舗コード（store_cd）ごとに
# 商品コード（product_cd）の最頻値を求めよ。
df_receipt %>%
  group_by(store_cd, product_cd) %>%
  summarise(product_cnt = length(receipt_no), .groups = "drop") %>%
  arrange(store_cd, desc(product_cnt)) %>%
  distinct(store_cd, .keep_all = T) %>%
  head(10)


## P-030 ##
# レシート明細データフレーム（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の標本分散を計算し、降順でTOP5を表示せよ。
# ★Rには直接的に標本分散を求める関数が無いため、ここでは不偏分散を計算している。
df_receipt %>%
  group_by(store_cd) %>%
  summarise(amount_var = var(amount)) %>%
  arrange(desc(amount_var)) %>%
  head(5)


## P-031 ##
# レシート明細データフレーム（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の標本標準偏差を計算し、降順でTOP5を表示せよ。
# ★Rには直接的に標本標準偏差を求める関数が無いため、ここでは不偏標準偏差を計算している。
df_receipt %>%
  group_by(store_cd) %>%
  summarise(amount_sd = sd(amount)) %>%
  arrange(desc(amount_sd)) %>%
  head(5)


## P-032 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）について、25％刻みで
# パーセンタイル値を求めよ。
df_receipt %>%
  summarise(amount_q1 = quantile(amount, 0.25),
            amount_q2 = quantile(amount, 0.75))


## P-033 ##
# レシート明細データフレーム（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の平均を計算し、330以上のものを抽出せよ。
df_receipt %>%
  group_by(store_cd) %>%
  summarise(amount_mean = mean(amount)) %>%
  arrange(desc(amount_mean)) %>%
  filter(amount_mean >= 330)


## P-034 ##
# レシート明細データフレーム（df_receipt）に対し、顧客ID（customer_id）ごとに
# 売上金額（amount）を合計して全顧客の平均を求めよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。
df_receipt %>%
  filter(!grepl("^Z", customer_id)) %>%
  group_by(customer_id) %>%
  summarise(amount_sum  = sum (amount)) %>%
  summarise(amount_mean = mean(amount_sum))


## P-035 ##
# レシート明細データフレーム（df_receipt）に対し、顧客ID（customer_id）ごとに
# 売上金額（amount）を合計して全顧客の平均を求め、平均以上に買い物をしている顧客を抽出せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。
# なお、データは10件だけ表示させれば良い。
average_amount <-
  df_receipt %>%
  filter(!grepl("^Z", customer_id)) %>%
  group_by(customer_id) %>%
  summarise(amount_sum  = sum (amount)) %>%
  summarise(amount_mean = mean(amount_sum)) %>%
  as.double()
df_receipt %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum (amount)) %>%
  filter(!grepl("^Z", customer_id), amount_sum >= average_amount) %>%
  head(10)
rm(average_amount)


## P-036 ##
# レシート明細データフレーム（df_receipt）と店舗データフレーム（df_store）を内部結合し、
# レシート明細データフレームの全項目と店舗データフレームの店舗名（store_name）を10件表示させよ。
inner_join(df_receipt,
           df_store[c("store_cd","store_name")],
           by="store_cd") %>%
  head(10)


## P-037 ##
# 商品データフレーム（df_product）とカテゴリデータフレーム（df_category）を内部結合し、
# 商品データフレームの全項目とカテゴリデータフレームの小区分名（category_small_name）を10件表示させよ。
inner_join(df_product,
           df_category[c("category_small_cd","category_small_name")],
           by="category_small_cd") %>%
  head(10)


## P-038 ##
# 顧客データフレーム（df_customer）とレシート明細データフレーム（df_receipt）から、
# 各顧客ごとの売上金額合計を求めよ。
# ただし、買い物の実績がない顧客については売上金額を0として表示させること。
# また、顧客は性別コード（gender_cd）が女性（1）であるものを対象とし、
# 非会員（顧客IDが'Z'から始まるもの）は除外すること。
# なお、結果は10件だけ表示させれば良い。
left_join(df_customer,
          df_receipt,
          by="customer_id") %>%
  group_by(customer_id, gender_cd) %>%
  summarise(amount = sum(amount), .groups = "drop") %>%
  filter(gender_cd == "1", !grepl("^Z", customer_id)) %>%
  replace_na(list(amount=0)) %>%
  head(10)


# P-039 ##
# レシート明細データフレーム（df_receipt）から売上日数の多い顧客の上位20件と、
# 売上金額合計の多い顧客の上位20件を抽出し、完全外部結合せよ。
# ただし、非会員（顧客IDが'Z'から始まるもの）は除外すること。
tmp1 <- df_receipt %>%
  filter(!grepl("^Z", customer_id)) %>%
  distinct(customer_id, sales_ymd) %>%
  group_by(customer_id) %>%
  summarise(date_cnt = length(sales_ymd)) %>%
  arrange(desc(date_cnt))
tmp2 <- df_receipt %>%
  filter(!grepl("^Z", customer_id)) %>%
  group_by(customer_id) %>%
  summarise(amount_sum = sum(amount)) %>%
  arrange(desc(amount_sum))
full_join(tmp1[1:20,], tmp2[1:20,], by="customer_id") %>%
  arrange(customer_id)


## P-040 ##
# 全ての店舗と全ての商品を組み合わせると何件のデータとなるか調査したい。
# 店舗（df_store）と商品（df_product）を直積した件数を計算せよ。
tidyr::crossing(df_store$store_cd,df_product$product_cd) %>% nrow()


## P-041 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を日付（sales_ymd）ごとに集計し、
# 前日からの売上金額増減を計算せよ。なお、計算結果は10件表示すればよい。
tmp1 <- df_receipt %>%
  mutate(sales_date = as.Date(as.character(sales_ymd),"%Y%m%d")) %>%
  group_by(sales_date) %>%
  summarise(amount = sum(amount))
tmp2 <- tmp1 %>%
  mutate(sales_date = sales_date+1) %>%
  rename(amount_pre = amount)
left_join(tmp1, tmp2, by="sales_date") %>%
  mutate(amount_diff = amount - amount_pre) %>%
  head(10)
rm(tmp1, tmp2) 


## P-042 ##
# レシート明細データフレーム（df_receipt）の売上金額（amount）を日付（sales_ymd）ごとに集計し、
# 各日付のデータに対し、１日前、２日前、３日前のデータを結合せよ。結果は10件表示すればよい。
tmp1 <- df_receipt %>%
  mutate(sales_date = as.Date(as.character(sales_ymd),"%Y%m%d")) %>%
  group_by(sales_date) %>%
  summarise(amount = sum(amount)) %>%
  mutate(after_1d = sales_date+1,
         after_2d = sales_date+2,
         after_3d = sales_date+3)
select(tmp1, sales_date, amount) %>%
  left_join(select(tmp1, sales_date = after_1d, amount_bef1 = amount), by="sales_date") %>%
  left_join(select(tmp1, sales_date = after_2d, amount_bef2 = amount), by="sales_date") %>%
  left_join(select(tmp1, sales_date = after_3d, amount_bef3 = amount), by="sales_date") %>%
  head(10)
rm(tmp1)


## P-043 ##
# レシート明細データフレーム（df_receipt）と顧客データフレーム（df_customer）を結合し、
# 性別（gender）と年代（ageから計算）ごとに売上金額（amount）を合計した売上サマリデータフレーム
#（df_sales_summary）を作成せよ。性別は0が男性、1が女性、9が不明を表すものとする。
# ただし、項目構成は年代、女性の売上金額、男性の売上金額、性別不明の売上金額の4項目とすること
#（縦に年代、横に性別のクロス集計）。また、年代は10歳ごとの階級とすること。
df_sales_summary <-
  inner_join(df_customer, df_receipt, by="customer_id") %>%
  mutate(generation = trunc(age/10)*10) %>%
  group_by(gender, generation) %>%
  summarise(amount = sum(amount), .groups = "drop") %>%
  pivot_wider(names_from = gender, values_from = amount)


## P-044 ##
# 前設問で作成した売上サマリデータフレーム（df_sales_summary）は性別の売上を横持ちさせたものであった。
# このデータフレームから性別を縦持ちさせ、年代、性別コード、売上金額の3項目に変換せよ。
# ただし、性別コードは男性を'00'、女性を'01'、不明を'99'とする。
df_sales_summary %>%
  pivot_longer(cols = c(女性,男性,不明), names_to = "gender", values_to = "amount") %>%
  rowwise() %>%
  mutate(gender_cd = switch (gender,
                             "男性" = "00",
                             "女性" = "01",
                             "不明" = "99")) %>%
  ungroup() %>%
  select(generation, gender_cd, gender, amount)

df_sales_summary %>%
  pivot_longer(cols = c(女性,男性,不明), names_to = "gender", values_to = "amount") %>%
  rowwise() %>%
  mutate(if      (gender == "男性") {gender_cd = "00"}
         else if (gender == "女性") {gender_cd = "01"}
         else if (gender == "不明") {gender_cd = "99"}) %>%
  ungroup() %>%
  select(generation, gender_cd, gender, amount)


## P-045 ##
# 顧客データフレーム（df_customer）の生年月日（birth_day）は日付型（Date）でデータを保有している。
# これをYYYYMMDD形式の文字列に変換し、顧客ID（customer_id）とともに抽出せよ。
# データは10件を抽出すれば良い。
df_customer[1:10,] %>%
  mutate(birth_ymd = format(birth_day,"%Y%m%d")) %>%
  select(customer_id, birth_ymd)


## P-046 ##
# 顧客データフレーム（df_customer）の申し込み日（application_date）はYYYYMMD形式の文字列型で
# データを保有している。
# これを日付型（dateやdatetime）に変換し、顧客ID（customer_id）とともに抽出せよ。
# データは10件を抽出すれば良い。
df_customer[1:10,] %>%
  mutate(application_dt = as.Date(application_date, "%Y%m%d")) %>%
  select(customer_id, application_dt)


## P-047 ##
# レシート明細データフレーム（df_receipt）の売上日（sales_ymd）はYYYYMMDD形式の数値型で
# データを保有している。
# これを日付型（dateやdatetime）に変換し、レシート番号(receipt_no)、レシートサブ番号（receipt_sub_no）
# とともに抽出せよ。データは10件を抽出すれば良い。
df_receipt[1:10,] %>%
  mutate(sales_dt = as.Date(as.character(sales_ymd),"%Y%m%d")) %>%
  select(receipt_no, receipt_sub_no, sales_dt)


## P-048 ##
# レシート明細データフレーム（df_receipt）の売上エポック秒（sales_epoch）は数値型のUNIX秒で
# データを保有している。これを日付型（dateやdatetime）に変換し、レシート番号(receipt_no)、
# レシートサブ番号（receipt_sub_no）とともに抽出せよ。データは10件を抽出すれば良い。
df_receipt[1:10,] %>%
  mutate(sales_dt = as.POSIXct(sales_epoch, origin="1970-01-01")) %>%
  select(receipt_no, receipt_sub_no, sales_dt, sales_ymd)


## P-049 ##
# レシート明細データフレーム（df_receipt）の売上エポック秒（sales_epoch）を日付型（timestamp型）に変換し、
# "年"だけ取り出してレシート番号(receipt_no)、レシートサブ番号（receipt_sub_no）とともに抽出せよ。
# データは10件を抽出すれば良い。
df_receipt[1:10,] %>%
  mutate(sales_year = format(as.POSIXct(sales_epoch, origin="1970-01-01"),"%Y")) %>%
  select(receipt_no, receipt_sub_no, sales_year, sales_ymd)


## P-050 ##
# レシート明細データフレーム（df_receipt）の売上エポック秒（sales_epoch）を日付型（timestamp型）に変換し、
# "月"だけ取り出してレシート番号(receipt_no)、レシートサブ番号（receipt_sub_no）とともに抽出せよ。
# なお、"月"は0埋め2桁で取り出すこと。データは10件を抽出すれば良い。
df_receipt[1:10,] %>%
  mutate(sales_month = format(as.POSIXct(sales_epoch, origin="1970-01-01"),"%m")) %>%
  select(receipt_no, receipt_sub_no, sales_month, sales_ymd)
