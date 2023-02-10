library(httpgd)
library(tidyverse)
## R-051: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上エポック秒を日付型に変換し、
# 「日」だけ取り出してレシート番号(receipt_no)、
# レシートサブ番号（receipt_sub_no）とともに10件表示せよ。
# なお、「日」は0埋め2桁で取り出すこと。
df_receipt[1:10, ] %>%
  mutate(sales_month = format(
    as.POSIXct(sales_epoch, origin = "1970-01-01"), "%d"
  )) %>%
  select(receipt_no, receipt_sub_no, sales_month, sales_ymd)

## R-051: ## ------------------------------------------------------------------

## R-052: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに
# 合計の上、売上金額合計に対して2,000円以下を0、2,000円より大きい金額を1に二値化し、
# 顧客ID、売上金額合計とともに10件表示せよ。ただし、顧客IDが"Z"から始まるのものは
# 非会員を表すため、除外して計算すること。
# 非会員を除外　->　group_by  -> 0, 1flag
df_receipt[!grepl("^Z", df_receipt$customer_id), ] %>%
  group_by(customer_id) %>%
  summarise(sum_amt = sum(amount)) %>%
  mutate(new_sum_amt = case_when(
    sum_amt <= 2000 ~ "0",
    sum_amt > 2000 ~ "1"
  )) %>%
  head()
## R-052: ## ------------------------------------------------------------------

## r-053: ## ------------------------------------------------------------------
# 顧客データ（df_customer）の郵便番号（postal_cd）に対し、
# 東京（先頭3桁が100〜209のもの）を1、それ以外のものを0に二値化せよ。
# さらにレシート明細データ（df_receipt）と結合し、全期間において売上実績のある
# 顧客数を、作成した二値ごとにカウントせよ。
# "二値化 -> join -> group_by -> count
df_customer %>%
  mutate(flg = if_else(
    between(as.numeric(substr(df_customer$postal_cd, 1, 3)), 100, 209), 1, 0
  )) %>%
  select(customer_id, postal_cd, flg) %>%
  inner_join(df_receipt, by = "customer_id", multiple = "all") %>%
  group_by(flg) %>%
  summarise(n = n_distinct(customer_id)) %>%
  head()
## ----------------------------------------------------------------------------

## r-054: ## ------------------------------------------------------------------
# 顧客データ（df_customer）の住所（address）は、埼玉県、千葉県、東京都、神奈川県の
# いずれかとなっている。都道府県毎にコード値を作成し、顧客ID、住所とともに10件表示せよ。
# 値は埼玉県を11、千葉県を12、東京都を13、神奈川県を14とすること。
df_customer %>%
  mutate(pref_cls = case_when(
    grepl("埼玉県", .$address) ~ 11,
    grepl("千葉県", .$address) ~ 12,
    grepl("東京都", .$address) ~ 13,
    grepl("神奈川県", .$address) ~ 14,
  )) %>%
  select(customer_id, address, pref_cls) %>%
  head()
## ----------------------------------------------------------------------------

## r-055: ## ------------------------------------------------------------------
# レシート明細データフレーム（df_receipt）の売上金額（amount）を顧客ID（customer_id）
# ごとに合計し、その合計金額の四分位点を求めよ。その上で、顧客ごとの売上金額合計に
# 対して以下の基準でカテゴリ値を作成し、顧客ID、売上金額と合計ともに表示せよ。
# カテゴリ値は上から順に1〜4とする。結果は10件表示させれば良い。
#   ・最小値以上第一四分位未満
#   ・第一四分位以上第二四分位未満
#   ・第二四分位以上第三四分位未満
#   ・第三四分位以上
df_receipt %>%
  group_by(customer_id) %>%
  summarise(sum_amt = sum(amount)) %>%
  mutate(rank = ntile(sum_amt, 4)) %>%
  head()
## ----------------------------------------------------------------------------

## r-056: ## ------------------------------------------------------------------
# 顧客データ（df_customer）の年齢（age）をもとに10歳刻みで年代を算出し、
# 顧客ID（customer_id）、生年月日（birth_day）とともに10件表示せよ。
# ただし、60歳以上は全て60歳代とすること。年代を表すカテゴリ名は任意とする。
df_customer %>%
  rowwise() %>%
  mutate(generation = min(trunc(age / 10) * 10, 60)) %>%
  select(customer_id, age, generation) %>%
  head(, n = 10)

df_customer2 <- df_customer %>%
  mutate(generation = trunc(age / 10) * 10) %>%
  mutate(generation = case_when(
    generation >= 60 ~ 60,
    generation < 60 ~ generation
  ))
df_customer2[1:3, ]
## ----------------------------------------------------------------------------

## r-057: ## ------------------------------------------------------------------
# 056の抽出結果と性別コード（gender_cd）により、新たに性別×年代の組み合わせを表す
# カテゴリデータを作成し、10件表示せよ。組み合わせを表すカテゴリの値は任意とする。
df_customer2 %>%
  mutate(gender_cd_gen = paste0(gender_cd, "-", generation)) %>%
  select(customer_id, gender_cd, generation, gender_cd_gen) %>%
  head()
## ----------------------------------------------------------------------------

## r-058: ## ------------------------------------------------------------------
# 顧客データ（df_customer）の性別コード（gender_cd）をダミー変数化し、
# 顧客ID（customer_id）とともに10件表示せよ。
df_customer %>%
  mutate(
    is_famale = if_else(gender_cd == 1, 1, 0),
    is_male = if_else(gender_cd == 0, 1, 0),
    other = if_else(gender_cd == 9, 1, 0)
  ) %>%
  select(customer_id, gender_cd, is_famale, is_male, other) %>%
  head()
## ----------------------------------------------------------------------------

## r-059: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに
# 合計し、売上金額合計を平均0、標準偏差1に標準化して顧客ID、売上金額合計とともに
# 10件表示せよ。標準化に使用する標準偏差は、分散の平方根、もしくは不偏分散の平方根の
# どちらでも良いものとする。ただし、顧客IDが"Z"から始まるのものは非会員を表すため、
# 除外して計算すること。
df_receipt[!grepl("^Z", df_receipt$customer_id), ] %>%
  group_by(customer_id) %>%
  summarise(amt_sum = sum(amount)) %>%
  mutate(scale1 = scale(amt_sum)) %>%
  select(customer_id, amt_sum, scale1) %>%
  head()
## ----------------------------------------------------------------------------

## r-060: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を顧客ID（customer_id）
# ごとに合計し、売上金額合計を最小値0、最大値1に正規化して顧客ID、
# 売上金額合計とともに10件表示せよ。ただし、顧客IDが"Z"から始まるのものは
# 非会員を表すため、除外して計算すること。
# df_receipt %>%
#   filter(substr(customer_id, 1, 1) != "Z") %>%
#   group_by(customer_id) %>%
#   summarise(sum_amt = sum(amount), .groups = "drop") %>%
#   mutate(norm = percent_rank(sum_amt)) %>%
#   head()
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(sum_amt = sum(amount), .groups = "drop") %>%
  mutate(norm = scale(
    sum_amt,
    center = min(sum_amt),
    scale = max(sum_amt) - min(sum_amt)
  )) %>%
  head()
## ----------------------------------------------------------------------------

## r-061: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに
# 合計し、売上金額合計を常用対数化（底10）して顧客ID、売上金額合計とともに10件表示せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(sum_amt = sum(amount), .groups = "drop") %>%
  mutate(log = log(sum_amt, 10)) %>%
  head()
## ----------------------------------------------------------------------------

## r-062: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに
# 合計し、売上金額合計を自然対数化（底e）して顧客ID、売上金額合計とともに10件表示せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。
df_receipt %>%
  filter(substr(customer_id, 1, 1) != "Z") %>%
  group_by(customer_id) %>%
  summarise(sum_amt = sum(amount), .groups = "drop") %>%
  mutate(exp_sum_amt = log(sum_amt)) %>%
  head()
## ----------------------------------------------------------------------------

## r-063: ## ------------------------------------------------------------------
# 商品データ（df_product）の単価（unit_price）と原価（unit_cost）から
# 各商品の利益額を算出し、結果を10件表示せよ。
df_product %>%
  mutate(profit = unit_price - unit_cost) %>%
  select(unit_price, unit_cost, profit) %>%
  head()
## ----------------------------------------------------------------------------

## r-064: ## ------------------------------------------------------------------
# 商品データ（df_product）の単価（unit_price）と原価（unit_cost）から、
# 各商品の利益率の全体平均を算出せよ。ただし、単価と原価には欠損が生じていることに注意せよ。
df_product %>%
  mutate(profit_rate = (unit_price - unit_cost) / unit_price * 100) %>%
  summarise(mean = mean(profit_rate, na.rm = TRUE))
## ----------------------------------------------------------------------------

## r-065: ## ------------------------------------------------------------------
# 商品データ（df_product）の各商品について、利益率が30%となる新たな単価を求めよ。
# ただし、1円未満は切り捨てること。そして結果を10件表示させ、
# 利益率がおよそ30％付近であることを確認せよ。
# ただし、単価（unit_price）と原価（unit_cost）には欠損が生じていることに注意せよ。
df_product %>%
  na.omit(unit_price, unit_cost) %>%
  mutate(
    new_unit_price = trunc((unit_cost) / 0.7),
    check = (new_unit_price - unit_cost) / new_unit_price
  ) %>%
  select(unit_price, unit_cost, new_unit_price, check) %>%
  head()
## ----------------------------------------------------------------------------

## r-066: ## ------------------------------------------------------------------
# 商品データ（df_product）の各商品について、利益率が30%となる新たな単価を求めよ。
# 今回は、1円未満を丸めること（四捨五入または偶数への丸めで良い）。
# そして結果を10件表示させ、利益率がおよそ30％付近であることを確認せよ。
# ただし、単価（unit_price）と原価（unit_cost）には欠損が生じていることに注意せよ。
df_product %>%
  na.omit(unit_price, unit_cost) %>%
  mutate(
    new_unit_price = round((unit_cost) / 0.7),
    check = (new_unit_price - unit_cost) / new_unit_price
  ) %>%
  select(unit_price, unit_cost, new_unit_price, check) %>%
  head()
## ----------------------------------------------------------------------------

## r-067: ## ------------------------------------------------------------------
# 商品データ（df_product）の各商品について、利益率が30%となる新たな単価を求めよ。
# 今回は、1円未満を切り上げること。そして結果を10件表示させ、
# 利益率がおよそ30％付近であることを確認せよ。
# ただし、単価（unit_price）と原価（unit_cost）には欠損が生じていることに注意せよ。
df_product %>%
  na.omit(unit_price, unit_cost) %>%
  mutate(
    new_unit_price = ceiling((unit_cost) / 0.7),
    check = (new_unit_price - unit_cost) / new_unit_price
  ) %>%
  select(unit_price, unit_cost, new_unit_price, check) %>%
  head()
## ----------------------------------------------------------------------------

## r-068: ## ------------------------------------------------------------------
# 商品データ（df_product）の各商品について、消費税率10％の税込み金額を求めよ。
# 1円未満の端数は切り捨てとし、結果を10件表示せよ。
# ただし、単価（unit_price）には欠損が生じていることに注意せよ。
df_product %>%
  na.omit(unit_price) %>%
  mutate(
    including_tax = trunc(unit_price * 1.1),
    check = including_tax / unit_price
  ) %>%
  select(unit_price, including_tax, check) %>%
  head()
## ----------------------------------------------------------------------------

## r-069: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）と商品データ（df_product）を結合し、
# 顧客毎に全商品の売上金額合計と、カテゴリ大区分コード（category_major_cd）が
# "07"（瓶詰缶詰）の売上金額合計を計算の上、両者の比率を求めよ。
# 抽出対象はカテゴリ大区分コード"07"（瓶詰缶詰）の売上実績がある顧客のみとし、結果を10件表示せよ。
df_tmp_1 <- df_receipt %>%
  group_by(customer_id) %>%
  summarise(sum_all = sum(amount))

df_tmp_2 <- inner_join(df_receipt,
  df_product[c("product_cd", "category_major_cd")],
  by = "product_cd") %>%
  filter(category_major_cd == "07") %>%
  group_by(customer_id) %>%
  summarise(sum_07 = sum(amount), .groups = "drop")
head(df_tmp_2)
inner_join(df_tmp_1, df_tmp_2, by = "customer_id") %>%
  mutate(sales_rate = sum_07 / sum_all) %>%
  head()
## ----------------------------------------------------------------------------
