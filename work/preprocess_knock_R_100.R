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
    scale = max(sum_amt) - min(sum_amt))) %>%
  head()
## ----------------------------------------------------------------------------

## r-061: ## ------------------------------------------------------------------
# レシート明細データ（df_receipt）の売上金額（amount）を顧客ID（customer_id）ごとに
# 合計し、売上金額合計を常用対数化（底10）して顧客ID、売上金額合計とともに10件表示せよ。
# ただし、顧客IDが"Z"から始まるのものは非会員を表すため、除外して計算すること。

## ----------------------------------------------------------------------------