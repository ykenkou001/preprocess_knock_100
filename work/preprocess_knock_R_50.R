library(dplyr)
library(tidyr)
# R-001:
# レシート明細データ（df_receipt）から全項目の先頭10件を表示し、どのよ
# なデータを保有しているか目視で確認せよ。
head(df_receipt, 5)

# R-002:
# レシート明細データ（df_receipt）から売上年月日（sales_ymd）、
# 顧客ID（customer_id）、商品コード（product_cd）、売上金額（amount）
# の順に列を指定し、10件表示せよ。
df_receipt[1:5, c("sales_ymd", "customer_id", "product_cd", "amount")]


# R-003:
# レシート明細データ（df_receipt）から売上年月日（sales_ymd）、
# 顧客ID（customer_id）、商品コード（product_cd）、売上金額（amount）の順に列を指定し、
# 10件表示せよ。ただし、sales_ymdsales_dateに項目名を変更しながら抽出すること。
head(select(
    df_receipt,
    sales_date = sales_ymd, customer_id, product_cd, amount
), 3)


# R-004:
# レシート明細データ（df_receipt）から売上日（sales_ymd）、
# 顧客ID（customer_id）、商品コード（product_cd）、売上金額（amount）の順に列を指定し、
# 以下の条件を満たすデータを抽出せよ。
# - 顧客ID（customer_id）が"CS018205000001"
df_receipt %>%
    select(sales_ymd, customer_id, product_cd, amount) %>%
    filter(customer_id == "CS018205000001")

# R-005:
# レシート明細データ（df_receipt）から売上日（sales_ymd）、
# 顧客ID（customer_id）、商品コード（product_cd）、売上金額（amount）
# の順に列を指定し、以下の全ての条件を満たすデータを抽出せよ。
# - 顧客ID（customer_id）が"CS018205000001"
# - 売上金額（amount）が1,000以上
df_receipt[c("sales_ymd", "customer_id", "product_cd", "amount")] %>%
    filter(customer_id == "CS018205000001" & amount >= 1000)

# R-006:
# レシート明細データ（df_receipt）から売上日（sales_ymd）、
# 顧客ID（customer_id）、商品コード（product_cd）、売上数量（quantity）、
# 売上金額（amount）の順に列を指定し、以下の全ての条件を満たすデータを抽出せよ。
# - 顧客ID（customer_id）が"CS018205000001"
# - 売上金額（amount）が1,000以上または売上数量（quantity）が5以上
df_receipt[
    c("sales_ymd", "customer_id", "product_cd", "quantity", "amount")
] %>%
    filter(customer_id == "CS018205000001" & (amount >= 1000 | quantity >= 5))

# R-007:
# レシート明細データ（df_receipt）から売上日（sales_ymd）、
# 顧客ID（customer_id）、商品コード（product_cd）、売上金額（amount）の順に列を指定し、
# 以下の全ての条件を満たすデータを抽出せよ。
# - 顧客ID（customer_id）が"CS018205000001"
# - 売上金額（amount）が1,000以上2,000以下
df_receipt[c("sales_ymd", "customer_id", "product_cd", "amount")] %>%
    filter(customer_id == "CS018205000001" & (amount >= 1000 & amount <= 2000))
df_receipt[c("sales_ymd", "customer_id", "product_cd", "amount")] %>%
    filter(customer_id == "CS018205000001" & (between(amount, 1000, 2000)))

# R-008:
# レシート明細データ（df_receipt）から売上日（sales_ymd）、
# 顧客ID（customer_id）、商品コード（product_cd）、売上金額（amount）
# の順に列を指定し、以下の全ての条件を満たすデータを抽出せよ。
# - 顧客ID（customer_id）が"CS018205000001"
# - 商品コード（product_cd）が"P071401019"以外
df_receipt[c("sales_ymd", "customer_id", "product_cd", "amount")] %>%
    filter(customer_id == "CS018205000001" & product_cd != "P071401019")

# R-009:
# 以下の処理において、出力結果を変えずにORをANDに書き換えよ。
# ----------------------------------------------------------------------------
# "df_store %>% filter(!(prefecture_cd == "13" | floor_area > 900))"
# ----------------------------------------------------------------------------
df_store %>% filter(!(prefecture_cd == "13" | floor_area > 900))

# R-010:
# 店舗データ（df_store）から、店舗コード（store_cd）が"S14"で
# 始まるものだけ全項目抽出し、10件表示せよ。
library(stringr)
filter(df_store, str_detect(store_cd, "^S14"))[1:3, ]

# R-011:
# 顧客データ（df_customer）から顧客ID（customer_id）の末尾が1のものだけ
# 全項目抽出し、10件表示せよ。
head(df_customer %>% filter(endsWith(customer_id, "1")), n = 3)

# R-012:
# 店舗データ（df_store）から、住所 (address) に"横浜市"が含まれるものだけ
# 全項目表示せよ。
head(df_store %>%
    filter(grepl(pattern = "横浜市", x = address)), 3)

# R-013:
# 顧客データ（df_customer）から、ステータスコード（status_cd）の先頭が
# アルファベットのA〜Fで始まるデータを全項目抽出し、10件表示せよ。
head(df_customer %>% filter(grepl("^[A-F]", status_cd)), 3)

# R-014:
# 顧客データ（df_customer）から、ステータスコード（status_cd）の末尾が
# 数字の1〜9で終わるデータを全項目抽出し、10件表示せよ。
df_customer %>%
    filter(grepl("[1-9]$", status_cd)) %>%
    head(., 3)

# R-015:
# 顧客データ（df_customer）から、ステータスコード（status_cd）の
# 先頭がアルファベットのA〜Fで始まり、末尾が数字の1〜9で終わるデータを全項目抽出し、
# 10件表示せよ。
df_customer %>%
    filter(grepl("[1-9]$", status_cd) & grepl("^[A-F]", status_cd)) %>%
    .[1:3, "status_cd"]

# R-016:
# 店舗データ（df_store）から、電話番号（tel_no）が3桁-3桁-4桁のデータを
# 全項目表示せよ。
df_store %>%
    filter(grepl("[0-9]{3}-[0-9]{3}-[0-9]{4}", tel_no)) %>%
    head(., 2)

# R-017:
# 顧客データ（df_customer）を生年月日（birth_day）で高齢順にソートし、
# 先頭から全項目を10件表示せよ。
head(df_customer[
    sort(df_customer$birth_day, decreasing = TRUE, index = TRUE)$ix,
], 3)

# R-018:
# 顧客データ（df_customer）を生年月日（birth_day）で若い順にソートし、
# 先頭から全項目を10件表示せよ。
head(
    df_customer[order(df_customer$birth_day, decreasing = TRUE), ],
    3
)

# R-019:
# レシート明細データ（df_receipt）に対し、1件あたりの売上金額（amount）が
# 高い順にランクを付与し、先頭から10件表示せよ。項目は顧客ID（customer_id）、
# 売上金額（amount）、付与したランクを表示させること。なお、売上金額（amount）が
# 等しい場合は同一順位を付与するものとする。
cols <- c("customer_id", "amount", "rank")
df_receipt[c("customer_id", "amount")] %>%
    mutate(rank = min_rank(desc(amount))) %>%
    arrange(rank) %>%
    head(.)

# R-020:
# レシート明細データ（df_receipt）に対し、1件あたりの売上金額（amount）が
# 高い順にランクを付与し、先頭から10件表示せよ。項目は顧客ID（customer_id）、
# 売上金額（amount）、付与したランクを表示させること。なお、売上金額（amount）
# が等しい場合でも別順位を付与すること。
df_receipt[c("customer_id", "amount")] %>%
    mutate(rank = row_number(desc(amount))) %>%
    arrange(rank) %>%
    head(.)

# R-021:
# レシート明細データ（df_receipt）に対し、件数をカウントせよ。
nrow(df_receipt)

# R-022: レシート明細データ（df_receipt）の顧客ID（customer_id）に対し、
# ユニーク件数をカウントせよ。
length(unique(df_receipt$customer_id))

# R-023:
# レシート明細データ（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）と売上数量（quantity）を合計せよ。
df_receipt %>%
    group_by(store_cd) %>%
    summarise(amount = sum(amount), quantity = sum(quantity)) %>%
    head(.)

# R-024:
# レシート明細データ（df_receipt）に対し、顧客ID（customer_id）ごとに
# 最も新しい売上年月日（sales_ymd）を求め、10件表示せよ。
df_receipt %>%
    group_by(customer_id) %>%
    # arrange(desc(sales_ymd)) %>%
    summarise(max_ymd = max(sales_ymd), .groups = "drop") %>%
    # nrow(.)
    .[1:10, ]

# R-025:
# レシート明細データ（df_receipt）に対し、顧客ID（customer_id）ごとに
# 最も古い売上年月日（sales_ymd）を求め、10件表示せよ。
df_receipt %>%
    group_by(customer_id) %>%
    summarise(min_ymd = min(sales_ymd), .groups = "drop") %>%
    .[1:10, ]

# R-026: レシート明細データ（df_receipt）に対し、顧客ID（customer_id）ごとに
# 最も新しい売上年月日（sales_ymd）と古い売上年月日を求め、両者が異なるデータを10件表示せよ。
df_receipt %>%
    group_by(customer_id) %>%
    mutate(
        max_ymd = max(sales_ymd),
        min_ymd = min(sales_ymd)
    ) %>%
    filter(., max_ymd != min_ymd) %>%
    .[1:10, c("sales_ymd", "max_ymd", "min_ymd")]


# R-027:
# 売上金額（amount）の平均を計算し、降順でTOP5を表示せよ。
df_receipt %>%
    group_by(store_cd) %>%
    summarise(mean = mean(amount)) %>%
    arrange(desc(mean)) %>%
    head(.)

# R-028:
# レシート明細データ（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の中央値を計算し、降順でTOP5を表示せよ。
df_receipt %>%
    group_by(store_cd) %>%
    summarise(median = median(amount)) %>%
    arrange(desc(median)) %>%
    head(.)

# R-029:
# レシート明細データ（df_receipt）に対し、店舗コード（store_cd）ごとに
# 商品コード（product_cd）の最頻値を求め、10件表示させよ。
df_receipt %>%
    group_by(store_cd, product_cd) %>%
    summarise(mode = n(), .groups = "drop_last") %>%
    filter(mode == max(mode)) %>% # mode == max(
    head(.)

# R-030:
# レシート明細データ（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の分散を計算し、降順で5件表示せよ。
df_receipt %>%
    group_by(store_cd) %>%
    summarise(var = var(amount)) %>%
    arrange(desc(var)) %>%
    head(.)

# R-031:
# レシート明細データ（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の標準偏差を計算し、降順で5件表示せよ。
df_receipt %>%
    group_by(store_cd) %>%
    summarise(sd = sd(amount)) %>%
    arrange(desc(sd)) %>%
    head(.)

# R-032:
# レシート明細データ（df_receipt）の売上金額（amount）について、
# 25％刻みでパーセンタイル値を求めよ。
quantile(df_receipt$amount)
df_receipt %>%
    summarise(
        q_25 = quantile(amount, 0.25),
        q_75 = quantile(amount, 0.75)
    )

# R-033:
# レシート明細データ（df_receipt）に対し、店舗コード（store_cd）ごとに
# 売上金額（amount）の平均を計算し、330以上のものを抽出せよ。
df_receipt %>%
    group_by(store_cd) %>%
    summarise(mean_amt = mean(amount), .groups = "drop") %>%
    filter(mean_amt > 330)

# R-034:
# レシート明細データ（df_receipt）に対し、顧客ID（customer_id）ごとに
# 売上金額（amount）を合計して全顧客の平均を求めよ。
# ただし、顧客IDが"Z"から始まるものは非会員を表すため、除外して計算すること。
df_receipt %>%
    group_by(customer_id) %>%
    summarise(sum_amt = sum(amount), .groups = "drop") %>%
    filter(!grepl("^Z", customer_id)) %>%
    summarise(mean = mean(sum_amt), .groups = "drop")

# R-035:
# レシート明細データ（df_receipt）に対し、顧客ID（customer_id）ごとに
# 売上金額（amount）を合計して全顧客の平均を求め、平均以上に買い物をしている顧客を
# 抽出し、10件表示せよ。ただし、顧客IDが"Z"から始まるものは非会員を表すため、
# 除外して計算すること。
amount_mean <- df_receipt %>%
    filter(!grepl("^Z", customer_id)) %>%
    group_by(customer_id) %>%
    summarise(sum_amt = sum(amount), .groups = "drop") %>%
    summarise(mean = mean(sum_amt), .groups = "drop")

amt_mean <- as.numeric(amount_mean)
amt_mean

df_receipt %>%
    group_by(customer_id) %>%
    summarise(amount = sum(amount), .groups = "drop") %>%
    filter(!grepl("^Z", customer_id), amount > amt_mean) %>%
    head()

# R-036:
# レシート明細データ（df_receipt）と店舗データ（df_store）を内部結合し、
# レシート明細データの全項目と店舗データの店舗名（store_name）を10件表示せよ。
common_col <- intersect(names(df_receipt), names(df_store))

left_join(df_receipt, df_store, by = common_col) %>%
    select(., names(df_receipt), "store_name") %>%
    head()

# R-037:
# 商品データ（df_product）とカテゴリデータ（df_category）を内部結合し、
# 商品データの全項目とカテゴリデータのカテゴリ小区分名（category_small_name）
# を10件表示せよ。

c_col <- intersect(names(df_product), names(df_category))
left_join(df_product, df_category[, c(c_col, "category_small_name")],
    by = c_col
) %>%
    select(., names(df_product), c_col, "category_small_name") %>%
    head()

# R-038: ---------------------------------------------------------------------
# 顧客データ（df_customer）とレシート明細データ（df_receipt）から、
# 顧客ごとの売上金額合計を求め、10件表示せよ。ただし、売上実績がない顧客については
# 売上金額を0として表示させること。また、顧客は性別コード（gender_cd）が女性（1）
# であるものを対象とし、非会員（顧客IDが"Z"から始まるもの）は除外すること。
df_customer[1:3, ]
df_receipt[1:3, ]
common_col <- intersect(names(df_customer), names(df_receipt))
common_col
head(df_customer, 3)
head(df_receipt, 3)
length(unique(df_customer$customer_id))
length(unique(df_receipt$customer_id))
dim(df_customer)
dim(df_receipt)
# df_customerにdf_receiptをleft_join -> filter -> group_by ,sum
left_join(df_customer, df_receipt, by = common_col, multiple = "all") %>%
    filter(!grepl("^Z", customer_id), gender_cd == 1, ) %>%
    group_by(customer_id) %>%
    summarise(sum_amt = sum(amount), .groups = "drop") %>%
    replace_na(list(sum_amt = 0)) %>%
    head()
# ----------------------------------------------------------------------------

# R-039: ---------------------------------------------------------------------
# レシート明細データ（df_receipt）から、売上日数の多い顧客の上位20件を抽出した
# データと、売上金額合計の多い顧客の上位20件を抽出したデータをそれぞれ作成し、
# さらにその2つを完全外部結合せよ。ただし、非会員（顧客IDが"Z"から始まるもの）は除外すること。
sales_top20 <- df_receipt %>%
    group_by(customer_id) %>%
    summarise(sales_top20 = n(), .groups = "drop") %>%
    arrange(., -sales_top20) %>%
    filter(!grepl("^Z", customer_id)) %>%
    .[1:20, ]
amount_top20 <- df_receipt %>%
    group_by(customer_id) %>%
    summarise(amount_20 = sum(amount), .groups = "drop") %>%
    arrange(., -amount_20) %>%
    filter(!grepl("^Z", customer_id)) %>%
    .[1:20, ]
# 完全外部結合
print(n = 10, full_join(sales_top20, amount_top20, by = "customer_id") %>%
    replace_na(list(sales_top20 = 0, amount_20 = 0)))
# R-039: ---------------------------------------------------------------------

## P-040 ## ------------------------------------------------------------------
# 全ての店舗と全ての商品を組み合わせると何件のデータとなるか調査したい。
# 店舗（df_store）と商品（df_product）を直積した件数を計算せよ。
nrow(expand.grid(unique(df_store$store_cd), unique(df_product$product_cd)))
## P-040 ## ------------------------------------------------------------------

## P-041 ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を日付（sales_ymd）ごとに
# 集計し、前回売上があった日からの売上金額増減を計算せよ。そして結果を10件表示せよ。
df_receipt %>%
    group_by(sales_ymd) %>%
    summarise(sum_amt = sum(amount)) %>%
    mutate(pre_amt = lead(sum_amt)) %>%
    mutate(diff_amt = pre_amt - sum_amt) %>%
    head()
## P-041 ## ------------------------------------------------------------------

## P-042 ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を日付（sales_ymd）ごとに
# 集計し、各日付のデータに対し、前回、前々回、3回前に売上があった日のデータを結合せよ。
# そして結果を10件表示せよ。
prev_1 <- df_receipt %>%
    group_by(sales_ymd) %>%
    summarise(sum_amt = sum(amount)) %>%
    mutate(
        prev_ymd = lag(sales_ymd, n = 1),
        prev_amt = lag(sum_amt, n = 1)
    )
prev_2 <- df_receipt %>%
    group_by(sales_ymd) %>%
    summarise(sum_amt = sum(amount)) %>%
    mutate(
        prev_ymd = lag(sales_ymd, n = 2),
        prev_amt = lag(sum_amt, n = 2)
    )
prev_3 <- df_receipt %>%
    group_by(sales_ymd) %>%
    summarise(sum_amt = sum(amount)) %>%
    mutate(
        prev_ymd = lag(sales_ymd, n = 3),
        prev_amt = lag(sum_amt, n = 3)
    )

concat_all <- bind_rows(prev_1, prev_2, prev_3)

result_df <- concat_all %>% # prev_1, prev_2, prev_3を縦に結合
    arrange(., sales_ymd) %>% # sales_ymdで並べ替え
    na.omit(.) # NAの行を削除
head(result_df, 20)
## P-042 ## ------------------------------------------------------------------

## p-043 ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）と顧客データ（df_customer）を結合し、
# 性別コード（gender_cd）と年代（ageから計算）ごとに売上金額（amount）を
# 合計した売上サマリデータを作成せよ。性別コードは0が男性、1が女性 9が不明を表すものとする。
# ただし、項目構成は年代、女性の売上金額、男性の売上金額、
# 性別不明の売上金額の4項目とすること（縦に年代、横に性別のクロス集計）。
# また、年代は10歳ごとの階級とすること。
# customer_idをキーにleft_joinし、gender_cd, ageでgroup_byしてamountをsum
df_sales_summary <-
    inner_join(
        df_customer, df_receipt,
        by = "customer_id", multiple = "all"
    ) %>%
    mutate(age_cls = trunc(age / 10) * 10) %>%
    group_by(gender, age_cls) %>%
    summarise(amount = sum(amount), .groups = "drop") %>%
    pivot_wider(names_from = gender, values_from = amount)
head(df_sales_summary)
## P-043 ## ------------------------------------------------------------------

## P-044 ## ------------------------------------------------------------------
# 043で作成した売上サマリデータ（df_sales_summary）は性別の売上を横持ちさせたものであった。
# このデータから性別を縦持ちさせ、年代、性別コード、売上金額の3項目に変換せよ。
# ただし、性別コードは男性を"00"、女性を"01"、不明を"99"とする。
tmp <- df_sales_summary %>%
    pivot_longer(
        cols = c("不明", "女性", "男性"),
        names_to = "gender", values_to = "amount"
    ) %>%
    mutate(gender = case_when(
        gender == "不明" ~ "99",
        gender == "男性" ~ "00",
        gender == "女性" ~ "01"
    ))
head(tmp)
## P-044 ## ------------------------------------------------------------------

## P-045 ## ------------------------------------------------------------------
#  顧客データ（df_customer）の生年月日（birth_day）は日付型でデータを保有している。
# これをYYYYMMDD形式の文字列に変換し、顧客ID（customer_id）とともに10件表示せよ。
df_customer %>%
    mutate(birth_day = gsub("-", "", .$birth_day)) %>%
    select(c("customer_id", "birth_day")) %>%
    head()
## P-045 ## ------------------------------------------------------------------

## P-046 ## ------------------------------------------------------------------
# 顧客データ（df_customer）の申し込み日（application_date）はyyyymmdd形式の
# 文字列型でデータを保有している。これを日付型に変換し、顧客id（customer_id）ととも
# に10件表示せよ。
class(df_customer$application_date)
df_customer %>%
    mutate(
        application_date = as.character(.$application_date)
    ) %>%
    mutate(
        application_date = as.Date(.$application_date, format = "%Y%m%d")
    ) %>%
    select(c("customer_id", "application_date")) %>%
    head()
## P-046 ## ------------------------------------------------------------------

## P-047 ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上日（sales_ymd）はYYYYMMDD形式の数値型で
# データを保有している。これを日付型に変換し、レシート番号(receipt_no)、
# レシートサブ番号（receipt_sub_no）とともに10件表示せよ。
# names(df_receipt)
df_receipt[1:10, ] %>%
    mutate(sales_ymd = as.character(.$sales_ymd, format = "%Y%m%d")) %>%
    mutate(sales_ymd = as.Date(.$sales_ymd, format = "%Y%m%d")) %>%
    select(c("receipt_no", "receipt_sub_no", "sales_ymd")) %>%
    head()
## P-047 ## ------------------------------------------------------------------

## P-048 ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上エポック秒（sales_epoch）は数値型のUNIX秒で
# データを保有している。これを日付型に変換し、レシート番号(receipt_no)、
# レシートサブ番号（receipt_sub_no）とともに10件表示せよ。
df_receipt %>%
    mutate(sales_ymd_date = as.POSIXct(.$sales_epoch, origin = "1970-1-1")) %>%
    select(c("receipt_no", "receipt_sub_no", "sales_ymd_date")) %>%
    head()
## P-048 ## ------------------------------------------------------------------

## P-049 ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上エポック秒（sales_epoch）を日付型に変換し、
# 「年」だけ取り出してレシート番号(receipt_no)、レシートサブ番号（receipt_sub_no）
# とともに10件表示せよ。
df_receipt %>%
    mutate(sales_ymd_date = as.POSIXct(.$sales_epoch, origin = "1970-1-1")) %>%
    mutate(sales_ymd_date = format(.$sales_ymd_date, "%Y")) %>%
    select(c("receipt_no", "receipt_sub_no", "sales_ymd_date")) %>%
    head()

## P-049 ## ------------------------------------------------------------------

## P-050 ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上エポック秒（sales_epoch）を日付型に変換し、
# 「月」だけ取り出してレシート番号(receipt_no)、レシートサブ番号（receipt_sub_no）
# とともに10件表示せよ。なお、「月」は0埋め2桁で取り出すこと。
df_receipt %>%
    mutate(sales_ymd_date = format(
        as.POSIXct(.$sales_epoch, origin = "1970-1-1"), "%m"
    )) %>%
    select(c("receipt_no", "receipt_sub_no", "sales_ymd_date")) %>%
    head()
## P-050 ## ------------------------------------------------------------------
